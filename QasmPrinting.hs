-- This file is part of Quipper. Copyright (C) 2011-2019. Please see the
-- file COPYRIGHT for a list of authors, copyright holders, licensing,
-- and other details. All rights reserved.
-- 
-- ======================================================================

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Pretty-printing of low-level quantum circuits.

--module QasmPrinting where

-- import other Quipper stuff
import Quipper.Utils.Auxiliary
import Quipper.Utils.Preview
import Quipper.Internal.Circuit
import Quipper.Internal.Generic
import Quipper.Internal.Monad
import Quipper.Internal.QData
import Quipper.Libraries.QuipperASCIIParser
import Quipper.Libraries.Unboxing

-- import other stuff
import Prelude
import Text.Printf
import Data.Char(isSpace, toLower)
import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import Graphics.EasyRender
import System.IO
import System.Environment
import System.Exit
import System.Directory
import Debug.Trace
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.Strict as MapS
import qualified Control.Monad.State.Lazy as SM

import qualified Data.IntMap as IntMap


import QasmTransformer 

-- ======================================================================
-- * Auxiliary functions

-- | Determine whether a named gate is self-inverse. The kind of a
-- gate is uniquely determined by its name, and the number of input
-- wires and generalized controls.
-- 
-- For now, we only recognize "X", "Y", "Z", "H", "not", "swap", and
-- "W" as self-inverse; it is not currently possible for user code to
-- extend this list.
self_inverse :: String -> [Wire] -> [Wire] -> Bool
self_inverse "X" [q] [] = True
self_inverse "Y" [q] [] = True
self_inverse "Z" [q] [] = True
self_inverse "H" [q] [] = True
self_inverse "not" [q] [] = True
self_inverse "swap" [q1,q2] [] = True
self_inverse "W" [q1,q2] [] = True
self_inverse _ _ _ = False

-- ======================================================================
-- * QASM representation of circuits

-- Qasm program Namespace and the correspondence between wire and
-- register, they are qasm program context.  We use state monad to
-- deal with context, i.e. our state monad has context as its
-- state. This way we don't need to explicitly pass the context
-- around.


-- | QasmGate Namespace is (gate_name, gate_declaration), for
-- primitive gates the declaration is empty. For subroutine, we will
-- add the subroutine here.
type QasmNamespace = Map String String


-- quantum/classical register, record the correspondence between
-- Qubit/Bit and QASM quantum/classical register.  e.g. wire 0 -->
-- in[0], 0 --> cin[0], for the moment, we don't record whether it is
-- signed.
type QCreg = Map Wire (String, Int, Wiretype) 

type Context = SM.State (QasmNamespace, QCreg)


-- $ Convert a circuit to QASM: one gate per line.

type WireTypeMap = IntMap.IntMap Wiretype

-- | Given a map of wiretypes, and a gate, update the wiretype in the map
-- if the gate changes it.
track_wiretype :: WireTypeMap -> Gate -> WireTypeMap
track_wiretype wtm (QInit    _ w _  ) = IntMap.insert w Qbit wtm
track_wiretype wtm (CInit    _ w _  ) = IntMap.insert w Cbit wtm
track_wiretype wtm (QMeas      w    ) = IntMap.insert w Cbit wtm
track_wiretype wtm (CGate    _ w _ _) = IntMap.insert w Cbit wtm
track_wiretype wtm (CGateInv _ w _ _) = IntMap.insert w Cbit wtm
track_wiretype wtm (QPrep      w _  ) = IntMap.insert w Qbit wtm
track_wiretype wtm (QUnprep    w _  ) = IntMap.insert w Cbit wtm
track_wiretype wtm (Subroutine boxid inv ws1 a1 ws2 a2 c ncf scf rep) = a2 `IntMap.union` wtm 
track_wiretype wtm _ = wtm

-- | Convert a 'BoxId' to the string in the format \"/name/\", shape \"/x/\".
qasm_of_boxid :: BoxId -> String
qasm_of_boxid (BoxId name shape) = (toLower (head name)) : drop 1 name -- name -- show name ++ ", shape " ++ show shape

-- | Generate an QASM representation of a control.  As controls are
-- stored as untyped wires, we can lookup the wiretype in the current
-- map and annotate the control if it's classical. 
qasm_render_control :: WireTypeMap -> Signed Wire -> String
qasm_render_control wtm (Signed w b) =
  (if b then "+" else "-") ++ show w ++ qasm_render_control_type wtype
  where 
    wtype = if (w `IntMap.member` wtm) then (wtm IntMap.! w) else Qbit
    qasm_render_control_type Qbit = ""
    qasm_render_control_type Cbit = "c"

-- | Generate an QASM representation of a list of controls.
qasm_render_controls :: WireTypeMap -> Controls -> String
qasm_render_controls wtm c =
  string_of_list " with controls=[" ", " "]" "" (qasm_render_control wtm) c

-- | Generate an QASM representation of a NoControlFlag
qasm_render_nocontrolflag :: NoControlFlag -> String
qasm_render_nocontrolflag False = ""
qasm_render_nocontrolflag True = " with nocontrol"

-- | To use qasm printing, we need first normalized the controls, so
-- if we only have quantume controls, the Signed Wiretype is
-- Wiretype. But to accommodate classical control, we still need Siged
-- Wiretype. On the other hand, QASM doesn't have a primitive gate
-- that has classical controls, it also doesn't allow to define
-- subroutine with classical input, so this is not used now, and only
-- for future extension.
type QuipGate = (String, Int, Int, Bool, [Signed Wiretype])

-- | QasmGate representation is (gate_name, gate_declaration), for
-- primitive gates the declaration is empty.
type QasmGate = (String, String)

-- record the correspondence between Quipper gates and Qasm supported
-- gates (including user-defined gates and primitive gates). This will
-- server as part of the Context.
type QQMap = Map QuipGate QasmGate



-- | To use qasm printing, we need first decompose to toffoli and
-- binary gate, so to be complete, we need to define all the
-- corresponding gates for them (toffoli, binary, unary), some of them
-- all qasm primitives, the rest cal be defined using qasm
-- primitives. But here, we only define part of them, if needed, we
-- can define more. Maybe for this to be a tool, I might need to add a
-- command line way to add new gate, instead of changing the code
-- here.
qqmap :: QQMap
qqmap = Map.fromList [
  (("not", 1, 0, False, []), ("x", "")),
  (("not", 1, 0, False, [Signed Qbit True]), ("cx", "")),
  (("not", 1, 0, False, [Signed Qbit True, Signed Qbit True]), ("ccx", "")),    
  (("X", 1, 0, False, []), ("x", "")),
  (("X", 1, 0, False, [Signed Qbit True]), ("cx", "")),
  (("X", 1, 0, False, [Signed Qbit True, Signed Qbit True]), ("ccx", "")),    
  (("H", 1, 0, False, []), ("h", "")),
  (("H", 1, 0, False, [Signed Qbit True]), ("ch", "")),  

  -- in principle, for self-inverse gate we don't need to provide
  -- "True" i.e. the inverse version of it.
  (("not", 1, 0, True, []), ("x", "")),
  (("not", 1, 0, True, [Signed Qbit True]), ("cx", "")),
  (("not", 1, 0, True, [Signed Qbit True, Signed Qbit True]), ("ccx", "")),    
  (("X", 1, 0, True, []), ("x", "")),
  (("X", 1, 0, True, [Signed Qbit True]), ("cx", "")),
  (("X", 1, 0, True, [Signed Qbit True, Signed Qbit True]), ("ccx", "")),    
  (("H", 1, 0, True, []), ("h", "")),
  (("H", 1, 0, True, [Signed Qbit True]), ("ch", "")),  
  
  (("Y", 1, 0, False, []), ("y", "")),  
  (("Z", 1, 0, False, []), ("z", "")),
  (("Y", 1, 0, True, []), ("y", "")),  
  (("Z", 1, 0, True, []), ("z", "")),
  (("S", 1, 0, False, []), ("s", "")),
  (("S", 1, 0, True, []), ("sdg", "")),
  (("T", 1, 0, False, []), ("t", "")),
  (("T", 1, 0, True, []), ("tdg", "")),

-- user-defined gate using qasm primitives. 
  (("E", 1, 0, False, []), ("my_e", "gate my_e a{rz(-3*pi/2) a; h a; x a;}\n")),
  (("E", 1, 0, True, []), ("my_e_inv", "gate my_e_inv a{x a; h a; rz(3*pi/2) a;}\n")),
  (("E", 1, 0, False, [Signed Qbit True]),
   ("my_ce", "gate my_e b,a{crz(-3*pi/2) b, a; ch b, a; cx b, a;}\n")),
  (("E", 1, 0, True, [Signed Qbit True]),
   ("my_ce_inv", "gate my_e_inv b,a{cx b, a; ch b,a; crz(3*pi/2) b, a;}\n")),

  (("omega", 1, 0, False, []), ("my_omega", "gate my_omega a{rz(-pi/2) a; s a;}\n")),
  (("omega", 1, 0, True, []), ("my_omega_inv", "gate my_omega_inv a{rz(pi/2) a; sdg a;}\n")),  
  (("omega", 1, 0, False, [Signed Qbit True]), ("my_comega", "gate my_comega c,a{swap c,a; t a; swap c,a;}\n")),
  (("omega", 1, 0, True, [Signed Qbit True]), ("my_comega_inv", "gate my_comega_inv c,a{swap c,a; tdg a; swap c,a;}\n")),


  (("V", 1, 0, False, []), ("my_v", "gate my_v a{h a; sdg a; h a;}\n")),
  (("V", 1, 0, True, []), ("my_v_inv", "gate my_v_inv a{h a; s a; h a;}\n")),
  (("V", 1, 0, False, [Signed Qbit True]), ("my_cv", "gate my_cv b, a{h a; tdg b; cx a,b; t b; tdg a; cx a,b; h a;}\n")),
  (("V", 1, 0, True, [Signed Qbit True]), ("my_cv_inv", "gate my_cv_inv a{h a; cx a,b; t a; tdg b; cx a,b; t b, h a;}\n")),

  -- Need add E gate.
  
  (("swap", 2, 0, False, []), ("swap", "")),
-- self_inverse  (("swap", 2, 0, True, []), ("swap", "")),
-- not needed, since we decompose it to binary plus toffoli
  --(("swap", 2, 0, False, [Signed Qbit True]), ("my_cswap", "gate c,b,a{ccx b,a;ccx c,a,b; ccx b,a;}\n ")),
  
-- the caller need to provide the timestep parameter to complete (%/2)
  (("exp(-i%Z)", 1, 0, False, []), ("rz", "")), -- = qasm rz(%/2)
  (("exp(-i%Z)", 1, 0, True, []), ("rz", "")), -- = qasm rz(-%/2)
  (("exp(-i%Z)", 1, 0, False, [Signed Qbit True]), ("crz", "")), -- = qasm crz(%/2)
  (("exp(-i%Z)", 1, 0, True, [Signed Qbit True]), ("crz", "")), -- = qasm crz(-%/2)

-- we don't need the controlled version for "R(2pi/%)" since
-- the transformer decompose the control to zero for phase gate.
  (("R(2pi/%)", 1, 0, False, []), ("p", "")), -- = p(2*pi*i/2^%)
  (("R(2pi/%)", 1, 0, True, []), ("p", "")), -- = p(-2*pi*i/2^%)
  
  -- controlled version for W is not needed, since we decompe it to
  -- binary and toffoli.
  (("W", 2, 0, False, []), ("my_w", "gate my_w a, b{cx b, a; cx a, b; ch b, a; cx a, b; cx b, a;}\n")),
--  (("W", 2, 0, True, []), ("my_w", "")),  self-inverse


  (("iX", 1, 0, False, []), ("my_ix", "gate my_ix a{rz(-pi) a; s a; s a; x a;}\n")),
  (("iX", 1, 0, True, []), ("my_ix_inv", "gate my_ix_inv a{rz(pi) a; s a; s a; x a;}\n")),
  (("iX", 1, 0, False, [Signed Qbit True]), ("my_cix", "gate my_ix c,a{swap c,a; s a; swap c,a; cx c a}\n")),
  (("iX", 1, 0, True, [Signed Qbit True]), ("my_cix_inv", "gate my_ix_inv c,a{swap c,a; sdg a; swap c,a; cx c a}\n"))
  ]


instance Eq (Signed Wiretype) where
  (==) (Signed q1 b1) (Signed q2 b2) = q1 == q2 && b1 == b2

    
instance Ord (Signed Wiretype) where
  compare (Signed Qbit b1) (Signed Qbit b2) = compare b1 b2
  compare (Signed Cbit b1) (Signed Cbit b2) = compare b1 b2  
  compare (Signed Qbit b1) (Signed Cbit b2) = GT
  compare (Signed Cbit b1) (Signed Qbit b2) = LT  

remove_controls :: QuipGate -> QuipGate
remove_controls (name, l1, l2, inv, c) = (name, l1, l2, inv, [])

qasm_name :: QuipGate -> String
qasm_name n@(name,_,_,_,c) = case Map.lookup n qqmap of
  Nothing ->
    case Map.lookup (remove_controls n) qqmap of
      Nothing -> error $ show name ++ " Gate not supported"
      Just (n',decl) -> "ctrl(" ++ (show $ length c) ++ ") @ " ++ n'
  Just (n',decl) -> n'

register_lookup :: Wire -> Context String
register_lookup w = do
  (ns,wr) <- SM.get
  let r = case Map.lookup w wr of
        Nothing -> error "undeclared register"
        Just (n,p,t) -> n ++ "[" ++ show p ++ "]"
  return r

register_lookup' :: Wire -> Context String
register_lookup' w = do
  (ns,wr) <- SM.get
  let r = case Map.lookup w wr of
        Nothing -> "not used"
        Just (n,p,t) -> n ++ "[" ++ show p ++ "]"
  return r



subroutine_arugument_lookup :: Wire -> Context String
subroutine_arugument_lookup w = do
  (ns,wr) <- SM.get
  let r = case Map.lookup (trace ( show w ++ "\n") w) wr of
        Nothing -> error "subroutine argument problem"
        Just (n,p,t) -> "a" ++ show p
  return r



register_add1 :: Wire -> Wiretype -> String -> Context String
register_add1 w t n = do
  (ns,wr) <- SM.get
  let wr' = Map.insert w (n,0,t) wr
  SM.put (ns,wr')
  return $ n ++ "[0]"


render_theta :: String -> InverseFlag -> Timestep -> String
render_theta name inv theta =
  show $ (if inv then -1.0 else 1.0) * case name of
    "exp(-i%Z)" -> theta/2.0
    "R(2pi/%)" -> 2.0*pi/theta
    _ -> error "unknown rotation gate"
  
-- | Generate an QASM representation of a single gate.
qasm_render_gate :: WireTypeMap -> Gate -> Context String
qasm_render_gate wtm (QGate "trace" _ _ _ _ _) = return ""
qasm_render_gate wtm (QGate name inv ws1 ws2 c ncf) = do
  rs <- sequence $ map register_lookup (wc ++ ws1 ++ ws2)
  return $ qasm_name (name, l1, l2, inv, c') 
    ++ string_of_list " " "," ";\n" "" id rs
  where
    inv' = inv && not (self_inverse name ws1 ws2)
    l1 = length ws1
    l2 = length ws2
    c' = map (\(Signed x b) -> (Signed (wtm IntMap.! x) b)) c
    wc = map (\(Signed x b) -> x) c
qasm_render_gate wtm (QRot name inv theta ws1 ws2 c ncf) = do
  rs <- sequence $ map register_lookup (wc ++ ws1 ++ ws2)  
  return $ qasm_name (name, l1, l2, inv, c') ++ "("++ (render_theta name inv theta) ++ ")"
    ++ string_of_list " " "," ";\n" "" id rs
  where
    inv' = inv && not (self_inverse name ws1 ws2)
    l1 = length ws1
    l2 = length ws2
    c' = map (\(Signed x b) -> (Signed (wtm IntMap.! x) b)) c
    wc = map (\(Signed x b) -> x) c

-- global phase is ignored. 
qasm_render_gate wtm (GPhase t ws c ncf) = return ""

-- all gates on classical data are not supported, since qasm do not
-- support operation on bit
qasm_render_gate wtm (CNot w c ncf) = error "classical operations on bit are not supported by QASM" 
qasm_render_gate wtm (CGate n w c ncf) = error "classical operations on bit are not supported by QASM" 
qasm_render_gate wtm (CGateInv n w c ncf) = error "classical operations on bit are not supported by QASM" 
qasm_render_gate wtm (CSwap w1 w2 c ncf) = error "classical operations on bit are not supported by QASM"

-- not supported 
qasm_render_gate wtm (QPrep w ncf) = error "Qprep is not supported in QASM" 
qasm_render_gate wtm (QUnprep w ncf) = error "QUnprep is not supported in QASM" 

-- QASM do not support locally init qubit, but we can use a global
-- register to make do.
qasm_render_gate wtm (QInit b w ncf) = do
  case b of
    False -> do
      l <- register_lookup' w
      case l of
        "not used" -> do
          r <- register_add1 w Qbit ("anc"++ show w)
          return $ "qreg anc" ++ show w ++ "[1];\n"
        _ -> return "" 
    True -> do
      l <- register_lookup' w
      case l of
        "not used" -> do
          r <- register_add1 w Qbit ("anc"++ show w)
          return $ "qreg anc" ++ show w ++ "[1];\nx anc"++ show w ++  "[0"  ++ "];\n"
        _ -> return $ "x anc"++ show w ++ "[0"  ++ "];\n"


-- QASM do not support locally init bit, but we can use a global bit
-- register to make do. QASM only allows to init a bit in 0 state.
qasm_render_gate wtm (CInit b w ncf) = do
  case b of
    True -> error "QASM only allows to init a bit in 0 state"
    False -> do
      r <- register_add1 w Cbit ("canc"++ show w)
      return $ "creg canc" ++ show w ++ "[1];\n"

-- QASM do not support Terminate a qubit, but we can use the qasm
-- "reset" primitive to make do.
qasm_render_gate wtm (QTerm b w ncf) = do
  r <- register_lookup w
  return $ "reset " ++ r ++ ";\n"
qasm_render_gate wtm (CTerm b w ncf) = error "classical operations on bits are not supported by QASM" 
qasm_render_gate wtm (QMeas w) = do
  r <- register_lookup w
  (ns,wr) <- SM.get
  let wr' = Map.delete w wr
  let wr'' = Map.insert w ("canc"++ show w,0,Cbit) wr'  
  return $ "creg canc" ++ show w ++ "[1];\nmeasure anc" ++ show w ++ "[0] -> canc" ++ show w ++ "[0];\n"

-- QASM do not support Discard a qubit, we just ignore this 
qasm_render_gate wtm (QDiscard w) = return ""
-- QASM do not support CDiscard a qubit, we just ignore this, since it
-- is euquivalent to no operation.
qasm_render_gate wtm (CDiscard w) = return ""
-- a hack, use barrier to represent 1, no barries represent 0
qasm_render_gate wtm (DTerm b w) = return $ case b of
  True -> "barrier w;\n"
  False -> ""

-- If a subroutine contain init, measure, terminating operation, (in
-- the common case, the output shape and input shape may mismatch due
-- to these operation), that would be a problem, since QASM do not
-- allow init, measure, termination operations in its subroutine. In
-- that case, we need inline this quipper subroutines first.
qasm_render_gate wtm (Subroutine boxid inv ws1 a1 ws2 a2 c ncf scf rep) = do
  rs <- sequence $ map register_lookup (wc ++ ws1)
  let (RepeatFlag rep1)  = rep
  let rep' = fromIntegral rep1
  return $ concat $
    (take rep' $ repeat (name ++ string_of_list " " "," ";\n" "" id rs))
  where
    inv' = inv && not (self_inverse name ws1 ws2)
    l1 = length ws1
    l2 = length ws2
    c' = map (\(Signed x b) -> (Signed (wtm IntMap.! x) b)) c
    wc = map (\(Signed x b) -> x) c
    name = qasm_of_boxid boxid
    show_rep = if rep == RepeatFlag 1 then "" else "(x" ++ show rep ++ ")"
qasm_render_gate wtm (Comment s inv ws) = do
  rs <- sequence $ map register_lookup (map fst ws)
  return $ "//// " ++ s ++
    "barrier" ++ string_of_list " " "," ";\n" "" id rs
    
  
-- | Generate an QASM representation of a gate list.
qasm_render_gatelist :: WireTypeMap -> [Gate] -> Context String
qasm_render_gatelist wtm []     = return ""
qasm_render_gatelist wtm (g:gs) = do
  gp <- (qasm_render_gate wtm g)
  tp <- (qasm_render_gatelist (track_wiretype wtm g) gs)
  return $ gp ++ tp


-- | Generate an QASM representation of a wiretype.
qasm_render_wiretype :: Wiretype -> String
qasm_render_wiretype Qbit = "Qbit"
qasm_render_wiretype Cbit = "Cbit"

-- | Generate an QASM representation of a type assignment.
qasm_render_typeas :: (Wire, Wiretype) -> String
qasm_render_typeas (w, t) =
  show w ++ ":" ++ qasm_render_wiretype t

-- | Generate an QASM representation of an arity, preceded by a title
-- (input or output).
qasm_render_arity :: String -> Arity -> String
qasm_render_arity title a =
  title ++ ": " ++ (string_of_list "" ", " "" "none" qasm_render_typeas (IntMap.toList a)) ++ "\n"

-- | Generate an QASM representation of an ordered arity, preceded by
-- a title (input or output).
qasm_render_oarity :: String -> [Wire] -> Arity -> String
qasm_render_oarity title ws a =
  title ++ ": " 
  ++ (string_of_list "" ", " "" "none" qasm_render_typeas tas_list) ++ "\n"
  where
    tas_list = [ (w, a IntMap.! w) | w <- ws ]






-- | Generate an QASM representation of a low-level ordered quantum
-- circuit.
qasm_of_ocircuit :: OCircuit -> Context String
qasm_of_ocircuit ocircuit = 
  (qasm_render_gatelist a1 gl)
    where
      OCircuit (win, circuit, wout) = ocircuit
      (a1, gl, a2, _) = circuit

-- | Generate an QASM representation of a low-level quantum circuit.
qasm_of_circuit :: Circuit -> Context String
qasm_of_circuit circuit = qasm_of_ocircuit ocircuit where
  ocircuit = OCircuit (w_in, circuit, w_out)
  (a1, _, a2, _) = circuit
  w_in = IntMap.keys a1
  w_out = IntMap.keys a2

-- | Generate an QASM representation of a low-level boxed quantum
-- circuit.
qasm_of_bcircuit :: BCircuit -> Context String
qasm_of_bcircuit (c,s) = do
  ds <- sequence $ map qasm_of_subroutine (Map.toList s)
  let decls = concat ds
  mc <- (qasm_of_circuit c) 
  return $ decls ++ mc



-- | Generate an QASM representation of a named subroutine.
qasm_of_subroutine :: (BoxId, TypedSubroutine) -> Context String
qasm_of_subroutine (boxid, TypedSubroutine ocirc input_strux output_strux ctrble) = do
  q <-  qasm_of_ocircuit ocirc
  let OCircuit (win, circuit, wout) = ocirc
  let args = string_of_list "" "," "" "" id  (map (\x -> "in" ++ show x) win)
  let q' = filter (\x -> x /= ']' && x /= '[') q
  let q'' = "gate " ++ qasm_of_boxid boxid ++ " " ++ args ++ "{\n" ++ q' ++ "}\n" 
  return q''

unJust (Just a) = a 

data QASMVersion = OpenQASM2 | OpenQASM3

qasm_of_dbcircuit :: QASMVersion -> ErrMsg -> DBCircuit a -> IO ()
qasm_of_dbcircuit version e dbcirc = do
  let header = case version of
        OpenQASM2 -> "OPENQASM 2.0;\ninclude \"qelib1.inc\";\n"
        OpenQASM3 -> "OPENQASM 3.0;\ninclude \"stdgates.inc\";\n"
  putStrLn header
  let regs = (if (IntMap.size qw == 0) then "" else "qreg in[" ++ show (IntMap.size qw) ++ "];") ++ if (length cw') > 0 then "\ncreg cin[" ++ show (IntMap.size cw) ++ "];\n" else ""
  putStrLn regs
  let wqr = Map.fromList [(i,("in", unJust ((i,wt) `elemIndex` qw'), Qbit)) | (i,wt) <- qw']
  let wcr = Map.fromList [(i,("in", unJust ((i,wt) `elemIndex` cw'), Cbit)) | (i,wt) <- cw']
  let wr = wqr `Map.union` wcr
  let qasm = SM.evalState (qasm_of_bcircuit bcirc) (Map.empty,  wr)
  putStrLn qasm
  where
    cw' = IntMap.toList cw
    qw' = IntMap.toList qw    
    (qw,cw) = IntMap.partition (==Qbit) inari
    (bcirc, _) = bcircuit_of_static_dbcircuit errmsg dbcirc
    (circ, ns)  = bcirc 
    (inari, gs, outari, ss)  = circ
    errmsg x = e ("operation not permitted during qasm printing: " ++ x)


print_dbcircuit_qasm = qasm_of_dbcircuit OpenQASM2
print_dbcircuit_qasm3 = qasm_of_dbcircuit OpenQASM3


-- | Print a usage message to 'stdout'.
usage :: IO ()
usage = do
  name <- getProgName
  putStrLn (header name)
    where header name =
            name ++ " compiles Quipper ASCII format circuit file to OpenQASM 2.0 or 3.0\n" ++ "program. Please using the following command:\n\n" ++ name ++ " <filename>\n" ++ name ++ " -3 <filename>\n" ++ name ++ " -inline <filename>\n" ++ name ++ " -3 -inline <filename>\n\n" ++ "By default OpenQASM 2.0 is generated unless -3 is passed. Since OpenQASM 2.0 only\n" ++ "supports gate subroutine (user-defined unitary gate) if your Quipper circuit has a\n" ++ "subroutine that is not a unitary, you need use -inline option to inline all the\n" ++ "subroutines."


main :: IO ()
main = do
  argv <- getArgs
  case argv of
    ["-h"]  -> handleHelp
    ["--help"] -> handleHelp
    [fn] -> do
      str <- readFile fn
      let (ins,circuit) = parse_circuit str
      let decomposed_circuit = qasm_generic circuit
      print_generic QASM decomposed_circuit ins
    ["-3", fn] -> do
      str <- readFile fn
      let (ins,circuit) = parse_circuit str
      let decomposed_circuit = qasm3_generic circuit
      print_generic QASM3 decomposed_circuit ins
    ["-inline", fn] -> handleInline fn
    ["--inline", fn] -> handleInline fn
    ["-3", "-inline", fn] -> handleInline3 fn
    ["-3", "--inline", fn] -> handleInline3 fn
    ["--inline", "-3", fn] -> handleInline3 fn
    ["-inline", "-3", fn] -> handleInline3 fn
    _ -> do
      hPutStrLn stderr ("Bad arguments or options. Try --help for more info.")
      exitFailure
  where
    handleHelp = do
      usage
      exitSuccess
    handleInline fn = do
      str <- readFile fn
      let (ins,circuit) = parse_circuit str
      let circuit' = unbox circuit      
      let decomposed_circuit = qasm_generic circuit'
      print_generic QASM decomposed_circuit ins
    handleInline3 fn = do
      str <- readFile fn
      let (ins,circuit) = parse_circuit str
      let circuit' = unbox circuit      
      let decomposed_circuit = qasm3_generic circuit'
      print_generic QASM3 decomposed_circuit ins



test a = do
  gate_X a

main' :: String -> IO ()
main' f = do
  str <- readFile f

  let (ins,circuit) = parse_circuit str
  let decomposed_circuit = qasm_generic circuit
  print_generic QASM decomposed_circuit ins
  print_generic Preview decomposed_circuit ins  

main'_inline :: String -> IO ()
main'_inline f = do
  str <- readFile f

  let (ins,circuit) = parse_circuit str
  let circuit' = unbox circuit
  let decomposed_circuit = qasm_generic circuit'
  print_generic QASM decomposed_circuit ins
  print_generic Preview decomposed_circuit ins  


-- ----------------------------------------------------------------------
-- * Printing to multiple formats

-- | Available output formats.

data Format = 
  EPS         -- ^ Encapsulated PostScript graphics.
  | PDF       -- ^ Portable Document Format. One circuit per page.
  | PS        -- ^ PostScript. One circuit per page.
  | ASCII     -- ^ A textual representation of circuits.
  | QASM      -- ^ OpenQASM 2.0
  | QASM3     -- ^ OpenQASM 3.0
  | Preview   -- ^ Don't print anything, but preview directly on screen (requires the external program /acroread/).
  | GateCount -- ^ Print statistics on gate counts.
  deriving Show
    
-- | A mapping from lower-case strings (to be used, e.g., with command
-- line options) to available formats.
format_enum :: [(String, Format)]
format_enum = [
  ("eps", EPS),
  ("pdf", PDF),
  ("ps", PS),
  ("postscript", PS),
  ("qasm", QASM),
  ("qasm3", QASM3),
  ("ascii", ASCII),  
  ("preview", Preview),
  ("gatecount", GateCount)
  ]
                    
-- | Print a low-level quantum circuit directly to the IO monad, using
-- the specified format.
print_dbcircuit :: Format -> ErrMsg -> DBCircuit a -> IO ()
print_dbcircuit QASM = print_dbcircuit_qasm
print_dbcircuit QASM3 = print_dbcircuit_qasm3


-- ======================================================================
-- * Generic printing

-- | Like 'print_unary', but also takes a stub error message.
print_errmsg :: (QCData qa) => ErrMsg -> Format -> (qa -> Circ b) -> qa -> IO ()
print_errmsg e format f shape = print_dbcircuit format e dbcircuit
  where 
    (in_bind, dbcircuit) = encapsulate_dynamic f shape

-- | Print a circuit generating function to the specified format; this
-- requires a shape parameter.
print_unary :: (QCData qa) => Format -> (qa -> Circ b) -> qa -> IO ()
print_unary = print_errmsg errmsg
  where 
    errmsg x = "print_unary: " ++ x

-- | Print a circuit generating function to the specified
-- format. Unlike 'print_unary', this can be applied to a
-- circuit-generating function in curried form with /n/ arguments, for
-- any /n >= 0/. It then requires /n/ shape parameters.
-- 
-- The type of this heavily overloaded function is difficult to
-- read. In more readable form, it has all of the following types:
-- 
-- > print_generic :: Format -> Circ qa -> IO ()
-- > print_generic :: (QCData qa) => Format -> (qa -> Circ qb) -> a -> IO ()
-- > print_generic :: (QCData qa, QCData qb) => Format -> (qa -> qb -> Circ qc) -> a -> b -> IO ()
-- 
-- and so forth.
 
print_generic :: (QCData qa, QCurry qfun qa b, Curry fun qa (IO())) => Format -> qfun -> fun
print_generic format f = g where
  f1 = quncurry f
  g1 = print_errmsg errmsg format f1
  g = mcurry g1
  errmsg x = "print_generic: " ++ x

-- | Like 'print_generic', but only works at simple types, and
-- therefore requires no shape parameters.
print_simple :: (QCData qa, QCurry qfun qa b, Curry fun qa (IO()), QCData_Simple qa) => Format -> qfun -> IO ()
print_simple format f = print_errmsg errmsg format f1 fs_shape where
  f1 = quncurry f
  errmsg x = "print_simple: " ++ x
