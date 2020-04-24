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
  (("R(2pi/%)", 1, 0, False, []), ("rz", "")), -- = rz(pi/2^(%+1)) upto a global phase
  (("R(2pi/%)", 1, 0, True, []), ("rz", "")), -- = rz(-pi/2^(%+1)) upto a global phase
  
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


qasm_name :: QuipGate -> String
qasm_name n@(name,_,_,_,_) = case Map.lookup n qqmap of
  Nothing -> error $ show name ++ " Gate not supported"
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
  return $ qasm_name (name, l1, l2, inv, c') ++ "("++ show theta ++ ")" 
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
          return $ "qreg anc" ++ show w ++ "[1];\nx anc[" ++ show w ++ "];\n"
        _ -> return $ "x anc[" ++ show w ++ "];\n"


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
  return $ "creg canc" ++ show w ++ "[1];\nmeasure -> canc" ++ show w ++ "[0];\n"

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

qasm_of_dbcircuit ::  ErrMsg -> DBCircuit a -> IO ()
qasm_of_dbcircuit e dbcirc = do
  putStrLn "OPENQASM 2.0;\ninclude \"qelib1.inc\";\n"
  let regs = "qreg in[" ++ show (IntMap.size qw) ++ "];" ++ if (length cw') > 0 then "\ncreg cin[" ++ show (IntMap.size cw) ++ "];\n" else "\n"
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


print_dbcircuit_qasm  = qasm_of_dbcircuit


-- | Print a usage message to 'stdout'.
usage :: IO ()
usage = do
  name <- getProgName
  putStrLn (header name)
    where header name =
            name ++ " compiles Quipper ASCII format circuit file\n" ++ "to OpenQasm 2.0 program. " ++ "Please using the following command:\n\n" ++ "QasmPrinting 'filename'\n" ++ "or, QasmPrinting -inline 'filename'\n\n" ++ "Since OpenQasm only supports gate subroutine (user-defined unitary gate)\n" ++ "if your Quipper circuit has a subroutine that is not a unitary,\n" ++ "you need use -inline option to inline all the subroutines."


main :: IO ()
main = do
  argv <- getArgs
--  let argv = words argv'
  case argv of
    ["-h"]  -> do
      usage
      exitSuccess
    ["--help"] -> do
      usage
      exitSuccess
    [fn] -> do
      str <- readFile fn
      let (ins,circuit) = parse_circuit str
      let decomposed_circuit = qasm_generic circuit
      print_generic QASM decomposed_circuit ins
    ["-inline", fn] -> do
      str <- readFile fn
      let (ins,circuit) = parse_circuit str
      let circuit' = unbox circuit      
      let decomposed_circuit = qasm_generic circuit'
      print_generic QASM decomposed_circuit ins
    ["--inline", fn] -> do
      str <- readFile fn
      let (ins,circuit) = parse_circuit str
      let circuit' = unbox circuit      
      let decomposed_circuit = qasm_generic circuit'
      print_generic QASM decomposed_circuit ins
    o : _ -> do
      hPutStrLn stderr ("Bad argument or option: '" ++ o ++ "'. Try --help for more info.")
      exitFailure



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


{-
-- ======================================================================
-- * Dynamic QASM representation of circuits

-- $
-- The dynamic QASM representation prints a circuit to standard
-- output in QASM format, just like the static QASM representation.
-- However, when a 'dynamic_lift' operation is encountered, it prompts
-- the user for the value of the corresponding bit. In effect, the
-- user is asked to act as the quantum device or simulator.   

-- | Write a prompt to get input from the user. Since the prompt
-- doesn't include a newline, the output must be flushed explicitly.
prompt :: String -> IO ()
prompt s = do
  putStr s
  hFlush stdout



-- | Interactively read a bit (either 0 or 1) from standard input.
-- This is intended for interactive user input, so it skips white
-- space until a 0 or 1 is encountered. In case the first
-- non-whitespace character isn't 0 or 1 or \'#\', the rest of the line
-- is ignored and the user is prompted to try again.
-- 
-- However, this also works for non-interactive input, so that the
-- input can be redirected from a file. In this case, the characters 0
-- and 1 and whitespace, including newlines, can be interspersed
-- freely. \'@#@\' starts a comment that extends until the end of the
-- line. 
getBit :: IO Bool
getBit = do
  c <- getChar
  case c of
    '0' -> return False
    '1' -> return True
    '#' -> do
      getLine
      getBit
    c | isSpace c -> getBit
    c -> do
      getLine
      prompt $ "# Expecting 0 or 1. Please try again: "
      getBit

-- | Embed a read-write computation in the 'IO' monad, by writing
-- gates to the terminal and interactively querying the user (or a
-- file on stdin) for dynamic liftings. We also update a 'Namespace'
-- while doing so, to collect any subroutines that are defined along
-- the way.

-- let a = context, i.e. a = (QasmNamespace, QCreg)
run_readwrite_qasm :: WireTypeMap -> ReadWrite a -> Namespace -> SM.StateT (QasmNamespace, QCreg) IO (a, Namespace)
run_readwrite_qasm wtm (RW_Return a) ns = return (a, ns)
run_readwrite_qasm wtm (RW_Write gate comp) ns = do
  (ns,qc) <- SM.get
  let (qasm,s) = SM.runState (qasm_render_gate wtm gate) (ns,qc)
  liftIO $ putStrLn qasm
  run_readwrite_qasm (track_wiretype wtm gate) comp ns
run_readwrite_qasm wtm (RW_Read w cont) ns = do
  prompt $ "# Value of wire " ++ show w ++ ": "
  bool <- getBit
  putStrLn $ "//// # Value: " ++ show bool
  run_readwrite_qasm wtm (cont bool) ns
run_readwrite_qasm wtm (RW_Subroutine name subroutine comp) ns = do
  let !ns' = map_provide name subroutine ns
  run_readwrite_qasm wtm comp ns'

-- | Interactively output a 'DBCircuit' to standard output. This
-- supports dynamic lifting by prompting the user for bit values when
-- a dynamic lifting operation is encountered. Effectively the user is
-- asked to behave like a quantum device.
print_dbcircuit_qasm :: ErrMsg -> DBCircuit a -> IO ()
print_dbcircuit_qasm _ (a0, comp) = do
  hSetBuffering stdout LineBuffering -- flush output after each line
  putStr (qasm_render_arity "Inputs" a0)
  ((a1, _, _),ns) <- run_readwrite_qasm a0 comp namespace_empty
  putStr (qasm_render_arity "Outputs" a1)
  sequence_ [ putStr $ qasm_of_subroutine subr | subr <- Map.toList ns ]
  putStr "\n"



-}



-- ----------------------------------------------------------------------
-- * Graphical representation of circuits

-- | The color white.
white :: Color
white = Color_Gray 1.0

-- | The color black.
black :: Color
black = Color_Gray 0.0

-- | A data type that holds all the customizable parameters.
data FormatStyle = FormatStyle {
  -- | The RenderFormat to use.
  renderformat :: RenderFormat,
  -- | The color of the background.
  backgroundcolor :: Color,
  -- | The color of the foreground (e.g. wires and gates).
  foregroundcolor :: Color,
  -- | Line width.
  linewidth :: Double,
  -- | Gap for double line representing classical bit.
  coffs :: Double,
  -- | Radius of dots for \"controlled\" gates.
  dotradius :: Double,
  -- | Radius of oplus for \"not\" gate.
  oplusradius :: Double,
  -- | Horizontal column width.
  xoff :: Double,
  -- | Difference between width of box and width of label.
  gatepad :: Double,
  -- | Height of labelled box.
  gateheight :: Double,
  -- | Width and height of \"cross\" for swap gate.
  crossradius :: Double,
  -- | Vertical shift for text labels.
  stringbase :: Double,
  -- | Width of \"bar\" bar.
  barwidth :: Double, 
  -- | Height of \"bar\" bar.
  barheight :: Double,
  -- | Width of \"D\" symbol.
  dwidth :: Double,
  -- | Height of \"D\" symbol.
  dheight :: Double,
  -- | Maximal width of a gate label.
  maxgatelabelwidth :: Double,
  -- | Maximal width of a wire label.
  maxlabelwidth :: Double,
  -- | Maximal width of a wire number.
  maxnumberwidth :: Double,
  -- | Font to use for labels on gates.
  gatefont :: Font,
  -- | Font to use for comments.
  commentfont :: Font,
  -- | Color to use for comments.
  commentcolor :: Color,
  -- | Font to use for labels.
  labelfont :: Font,
  -- | Color to use for labels.
  labelcolor :: Color,
  -- | Font to use for numbers.
  numberfont :: Font,
  -- | Color to use for numbers.
  numbercolor :: Color,
  -- | Whether to label each subroutine call with shape parameters
  subroutineshape :: Bool
} deriving Show

-- | A RenderFormat consisting of some default parameters, 
-- along with the given RenderFormat.
defaultStyle :: RenderFormat -> FormatStyle
defaultStyle rf = FormatStyle {
  renderformat = rf,
  backgroundcolor = white,
  foregroundcolor = black,
  linewidth = 0.02, 
  coffs = 0.03,
  dotradius  = 0.15,
  oplusradius = 0.25,
  xoff = 1.5,
  gatepad = 0.3, 
  gateheight  = 0.8,
  crossradius = 0.2,
  stringbase = 0.25,
  barwidth = 0.1,
  barheight = 0.5,
  dwidth = 0.3,
  dheight = 0.4,
  maxgatelabelwidth = 1.1,
  maxlabelwidth = 0.7,
  maxnumberwidth = 0.7,
  gatefont = Font TimesRoman 0.5,
  commentfont = Font TimesRoman 0.3,
  commentcolor = Color_RGB 1 0.2 0.2,
  labelfont = Font TimesRoman 0.3,
  labelcolor = Color_RGB 0 0 1,
  numberfont = Font Helvetica 0.5,
  numbercolor = Color_RGB 0 0.7 0,
  subroutineshape = True
}

-- | The default PDF Style.
pdf :: FormatStyle
pdf = defaultStyle Format_PDF

-- | The default EPS Style.
eps :: FormatStyle
eps = defaultStyle (Format_EPS 1)

-- | The default PS Style.
ps :: FormatStyle
ps = defaultStyle (Format_PS)

-- ----------------------------------------------------------------------
-- ** General-purpose PostScript functions

-- | Escape special characters in a string literal.
ps_escape :: String -> String
ps_escape [] = []
ps_escape ('\\' : t) = '\\' : '\\' : ps_escape t
ps_escape ('('  : t) = '\\' : '('  : ps_escape t
ps_escape (')'  : t) = '\\' : ')'  : ps_escape t
ps_escape (h : t)    = h : ps_escape t

-- ----------------------------------------------------------------------
-- ** String formatting

-- | Convert a 'BoxId' to the string in the format \"/name/, shape /x/\".
string_of_boxid :: BoxId -> String
string_of_boxid (BoxId name shape) = name ++ ", shape " ++ shape

-- ----------------------------------------------------------------------
-- ** Functions for dealing with x-coordinates

-- | Pre-processing: figure out the /x/-column of each gate. Returns
-- (/n/,/xgs/) where /xgs/ is a list of ('Gate', 'X') pairs, and
-- /n/ is the rightmost /x/-coordinate of the circuit. Here we start
-- from /x0/ and use constant step /xoff/ taken from the 'FormatStyle'.
assign_x_coordinates :: FormatStyle -> [Gate] -> X -> (X, [(Gate, X)])
assign_x_coordinates fs gs x0 =
  let ((x,ws), xgs) = mapAccumL (\ (x, ws) g ->
        -- count the wires attached to the gate. If there is precisely
        -- one (unary gate), merge it with adjacent unary gates. Do
        -- not merge comments.
        let merge = case (g, wirelist_of_gate g) of
              (Comment _ _ _, _) -> Nothing
              (_, [w]) -> Just w
              (_, _) -> Nothing
        in
        case merge of
          Just w ->
            if not (w `elem` ws) then
              ((x, w:ws), (g, x))
            else
              ((x + (xoff fs), [w]), (g, x + (xoff fs)))
          _ ->
            if ws == [] then
              ((x + (xoff fs), []), (g, x))
            else
              ((x + 2.0 * (xoff fs), []), (g, x + (xoff fs)))
        ) (x0, []) gs
  in
   if ws == [] then
     (x, xgs)
   else
     (x + (xoff fs), xgs)

-- | A 'Xarity' is a map from wire id's to pairs of a wiretype and a
-- starting /x/-coordinate.
type Xarity = Map Wire (Wiretype, X)

-- | Figure out how a gate at coordinate /x/ affects the current 'Xarity'.
-- Return a pair (/term/, /new/), where /term/ is the 'Xarity' of wires
-- terminated by this gate, and /new/ is the outgoing 'Xarity' of this
-- gate.
update_xarity :: Xarity -> Gate -> X -> (Xarity, Xarity)
update_xarity xarity gate x =
  let (win, wout) = gate_arity gate
      safe_lookup xarity w = 
        case Map.lookup w xarity of 
          Just x -> x
          Nothing -> (Qbit, x) -- error ("update_xarity: the wire " ++ show w ++ " does not exist. In the gate:\n" ++ qasm_render_gate gate)
      (win', wout') = (win \\ wout, wout \\ win)
      -- extract terminating wires from xarity
      xarity_term = foldl (\xar (w,_) -> Map.insert w (xarity `safe_lookup` w) xar) Map.empty win' 
      -- extract continuing wires from xarity
      xarity_cont = foldl (\xar (w,_) -> Map.delete w xar) xarity win'
      -- add new wires to xarity_cont
      xarity_new = foldl (\xar (w,t) -> Map.insert w (t,x) xar) xarity_cont wout'
  in
   (xarity_term, xarity_new)

-- ----------------------------------------------------------------------
-- ** Low-level drawing functions

-- | @'render_line' x0 y0 x1 y1@: Draw a line from (/x0/, /y0/)
-- to (/x1/, /y1/). In case of a zero-length line, draw nothing.
render_line :: X -> Y -> X -> Y -> Draw ()
render_line x0 y0 x1 y1 | x0 == x1 && y0 == y1 = return ()
render_line x0 y0 x1 y1 = draw_subroutine alt $ do
  moveto x0 y0
  lineto x1 y1
  stroke
  where
    alt = [custom_ps $ printf "%f %f %f %f line\n" x0 y0 x1 y1]

-- | @'render_dot' x y@: Draw a filled control dot at (/x/,/y/).
render_dot :: FormatStyle -> X -> Y -> Draw ()
render_dot fs x y = draw_subroutine alt $ do
  arc x y (dotradius fs) 0 360
  fill (foregroundcolor fs)
  where
    alt = [custom_ps $ printf "%f %f dot\n" x y]

-- | @'render_circle' x y@: Draw an empty control dot at
-- (/x/,/y/).
render_circle :: FormatStyle -> X -> Y -> Draw ()
render_circle fs x y = draw_subroutine alt $ do
  arc x y (dotradius fs) 0 360
  fillstroke (backgroundcolor fs)
  where
    alt = [custom_ps $ printf "%f %f circ\n" x y]

-- | @'render_not' x y@: Draw a \"not\" gate at (/x/,/y/).
render_not :: FormatStyle -> X -> Y -> Draw ()
render_not fs x y = draw_subroutine alt $ do
  arc x y (oplusradius fs) 0 360
  fillstroke (backgroundcolor fs)
  render_line (x-(oplusradius fs)) y (x+(oplusradius fs)) y
  render_line x (y-(oplusradius fs)) x (y+(oplusradius fs))
  where
    alt = [custom_ps $ printf "%f %f oplus\n" x y]

-- | @'render_swap' x y@: Draw a cross (swap gate component) at
--  (/x/,/y/).
render_swap :: FormatStyle -> X -> Y -> Draw ()
render_swap fs x y = draw_subroutine alt $ do
  render_line (x-(crossradius fs)) (y-(crossradius fs)) (x+(crossradius fs)) (y+(crossradius fs))
  render_line (x-(crossradius fs)) (y+(crossradius fs)) (x+(crossradius fs)) (y-(crossradius fs))
  where  
    alt = [custom_ps $ printf "%f %f cross\n" x y]

-- | @'render_bar' x y@: Draw an init/term bar at (/x/,/y/).
render_bar :: FormatStyle -> X -> Y -> Draw ()
render_bar fs x y = draw_subroutine alt $ do
  rectangle (x - (barwidth fs)/2) (y - (barheight fs)/2) (barwidth fs) (barheight fs)
  fill (foregroundcolor fs)
  where
    alt = [custom_ps $ printf "%f %f bar\n" x y]

-- | @'render_bar' x y@: Draw a dterm bar at (/x/,/y/).
render_dbar :: FormatStyle -> X -> Y -> Draw ()
render_dbar fs x y = draw_subroutine alt $ do
  block $ do
    translate (x+(barwidth fs)/2) y
    scale (dwidth fs) (dheight fs)
    moveto (-1) (-0.5)
    arc_append (-0.5) 0 0.5 (-90) 90
    lineto (-1) 0.5
    closepath
    fill (foregroundcolor fs)
  where
    alt = [custom_ps $ printf "%f %f dbar\n" x y]

-- | @'render_init' name x y@: Draw an \"init\" gate at
-- (/x/,/y/), with state /name/.
render_init :: FormatStyle -> String -> X -> Y -> Draw ()
render_init fs name x y = draw_subroutine alt $ do
  render_bar fs x y
  textbox align_right (gatefont fs) (foregroundcolor fs) (x-(xoff fs)/2+(gatepad fs)/2) y (x-(gatepad fs)/2) y (stringbase fs) name
  where
    alt = [custom_ps $ printf "(%s) %f %f init\n" (ps_escape name) x y]

-- | @'render_term' name x y@: Draw a \"term\" gate at
-- (/x/,/y/), with state /name/.
render_term :: FormatStyle -> String -> X -> Y -> Draw ()
render_term fs name x y = draw_subroutine alt $ do
  render_bar fs x y
  textbox align_left (gatefont fs) (foregroundcolor fs) (x+(gatepad fs)/2) y (x+(xoff fs)/2-(gatepad fs)/2) y (stringbase fs) name
  where
    alt = [custom_ps $ printf "(%s) %f %f term\n" (ps_escape name) x y]

-- | @'render_dterm' name x y@: Draw a \"dterm\" gate at
-- (/x/,/y/), with state /name/.
render_dterm :: FormatStyle -> String -> X -> Y -> Draw ()
render_dterm fs name x y = draw_subroutine alt $ do
  render_dbar fs x y
  textbox align_left (gatefont fs) (foregroundcolor fs) (x+(gatepad fs)/2) y (x+(xoff fs)/2-(gatepad fs)/2) y (stringbase fs) name
  where
    alt = [custom_ps $ printf "(%s) %f %f dterm\n" (ps_escape name) x y]

-- | @'render_namedgate' name inv x y@: draw a named box centered at
-- (/x/,/y/). If /inv/ = 'True', append an \"inverse\" symbol to the
-- end of the name.
render_namedgate :: FormatStyle -> String -> InverseFlag -> X -> Y -> Draw ()
render_namedgate fs name inv x y = draw_subroutine alt $ do
  rectangle (x-gatewidth/2) (y-(gateheight fs)/2) gatewidth (gateheight fs)
  fillstroke (backgroundcolor fs)
  textbox align_center (gatefont fs) (foregroundcolor fs) (x-labelwidth/2) y (x+labelwidth/2) y (stringbase fs) name'
  where
    alt = [custom_ps $ printf "(%s) %f %f gate\n" (ps_escape name') x y]
    name' = name ++ optional inv "*"
    w = text_width (gatefont fs) name'
    labelwidth = min w (maxgatelabelwidth fs)
    gatewidth = labelwidth + (gatepad fs)
            
-- | @'render_gphasegate' name x y@: draw a global phase gate
-- centered at (/x/,/y/).
render_gphasegate :: FormatStyle -> String -> X -> Y -> Draw ()
render_gphasegate fs name x y = draw_subroutine alt $ do
  render_circgate fs name x (y-0.5)
  where
    alt = [custom_ps $ printf "(%s) %f %f gphase\n" (ps_escape name) x y]

-- | @'render_circgate' name x y@: draw a named oval centered at
-- (/x/,/y/).
render_circgate :: FormatStyle -> String -> X -> Y -> Draw ()
render_circgate fs name x y = draw_subroutine alt $ do
  oval x y (0.5*gatewidth) (0.4*(gateheight fs))
  fillstroke (backgroundcolor fs)
  textbox align_center (gatefont fs) (foregroundcolor fs) (x-labelwidth/2) y (x+labelwidth/2) y (stringbase fs) name
  where
    alt = [custom_ps $ printf "(%s) %f %f circgate\n" (ps_escape name) x y]
    w = text_width (gatefont fs) name
    labelwidth = min w (maxgatelabelwidth fs)
    gatewidth = labelwidth + (gatepad fs)
    
-- | @'render_blankgate' name x y@: draw an empty box centered
-- at (/x/,/y/), big enough to hold /name/.
render_blankgate :: FormatStyle -> String -> X -> Y -> Draw ()
render_blankgate fs name x y = draw_subroutine alt $ do
  rectangle (x-gatewidth/2) (y-(gateheight fs)/2) gatewidth (gateheight fs)
  fillstroke (backgroundcolor fs)
  where
    alt = [custom_ps $ printf "(%s) %f %f box\n" (ps_escape name) x y]
    w = text_width (gatefont fs) name
    labelwidth = min w (maxgatelabelwidth fs)
    gatewidth = labelwidth + (gatepad fs)

-- | @'render_comment' center s x y m@: draw the given string
-- vertically, with the top of the string near the given
-- /y/-coordinate. If /center/=='True', center it at the
-- /x/-coordinate, else move it just to the left of the
-- /x/-coordinate. /m/ is the maximum height allowed for the comment.
render_comment :: FormatStyle -> Bool -> String -> X -> Y -> Y -> Draw ()
render_comment fs center s x y maxh = draw_subroutine alt $ do
  textbox align_right (commentfont fs) (commentcolor fs) x (y-maxh) x (y+0.4) b s
  where
    alt = [custom_ps $ printf "(%s) %f %f %f %f comment\n" (ps_escape s) x y maxh yshift]
    b = if center then 0.15 else -0.25
    yshift = -b * nominalsize (commentfont fs)

-- | @'render_label' center s x y@: draw the given label just above
-- the given point. If /center/=='True', center it at the
-- /x/-coordinate, else move it just to the right of the
-- /x/-coordinate.
render_label :: FormatStyle -> Bool -> String -> X -> Y -> Draw ()
render_label fs True s x y = draw_subroutine alt $ do
  textbox align_center (labelfont fs) (labelcolor fs) (x-(maxlabelwidth fs)) y' (x+(maxlabelwidth fs)) y' (-0.5) s
  where
    alt = [custom_ps $ printf "(%s) %f %f clabel\n" (ps_escape s) x y']
    y' = y + 0.5 * (coffs fs)
render_label fs False s x y = draw_subroutine alt $ do
  textbox align_left (labelfont fs) (labelcolor fs) x y' (x+(maxlabelwidth fs)) y' (-0.5) s
  where
    alt = [custom_ps $ printf "(%s) %f %f rlabel\n" (ps_escape s) x y']
    y' = y + 0.5 * (coffs fs)
    
-- | Render the number at the given point (/x/,/y/). If the boolean
-- argument is 'True', put the number to the right of /x/, else to the left. 
render_number :: FormatStyle -> Int -> Bool -> X -> Y -> Draw ()
render_number fs i True x y = draw_subroutine alt $ do
  textbox align_left (numberfont fs) (numbercolor fs) (x+0.2) y (x+0.2+(maxnumberwidth fs)) y (stringbase fs) (show i)
  where
    alt = [custom_ps $ printf "(%s) %f %f rnumber\n" (ps_escape (show i)) x y]
render_number fs i False x y = draw_subroutine alt $ do
  textbox align_right (numberfont fs) (numbercolor fs) (x-0.2-(maxnumberwidth fs)) y (x-0.2) y (stringbase fs) (show i)
  where
    alt = [custom_ps $ printf "(%s) %f %f lnumber\n" (ps_escape (show i)) x y]

-- ----------------------------------------------------------------------
-- ** Higher-level rendering functions

-- | Render a horizontal wire from /x/-coordinates /oldx/ to /x/,
-- using /t/ as the type and figuring out the /y/-coordinate from /ys/
-- and /w/. Append to the given string. If the parameters are invalid
-- (/w/ not in /ys/), throw an error.
render_typeas :: FormatStyle -> Map Wire Y -> X -> X -> Wire -> Wiretype -> Draw ()
render_typeas fs ys oldx x w t =
  let y = ys Map.! w in
  case t of
    Qbit -> do
      render_line oldx y x y
    Cbit -> do
      render_line oldx (y + (coffs fs)) x (y + (coffs fs))
      render_line oldx (y - (coffs fs)) x (y - (coffs fs))

-- | Render a bunch of horizontal wires from their respective starting
-- 'Xarity' to /x/.
render_xarity :: FormatStyle -> Map Wire Y -> Xarity -> X -> Draw ()
render_xarity fs ys xarity x = do
  sequence_ [ render_typeas fs ys oldx x w t | (w,(t,oldx)) <- Map.toList xarity ]

-- | Format a floating point number in concise form, with limited
-- accuracy.
dshow :: Double -> String
dshow dbl = 
  if abs dbl < 0.01 
  then
    printf "%.1e" dbl
  else
    (reverse . strip . reverse) (printf "%.3f" dbl)
      where
        strip [] = []
        strip ('.' : t) = t
        strip ('0' : t) = strip t
        strip t = t
        
-- | @'render_controlwire' /x/ /ys/ /ws/ /c/@: 
-- Render the line connecting all the box components and all the
-- control dots of some gate. 
-- 
-- Parameters: /x/ is the current /x/-coordinate, /ys/ is an indexed
-- array of /y/-coordinates, /ws/ is the set of wires for boxes, and
-- /c/ is a list of controls.
render_controlwire :: X -> Map Wire Y -> [Wire] -> Controls -> Draw ()
render_controlwire x ys ws c =
  case ws of
    [] -> return ()
    w:ws -> render_line x y0 x y1      
      where
        ymap w = ys Map.! w
        y = ymap w
        cy = map (\(Signed w _) -> ymap w) c
        yy = map (\w -> ymap w) ws
        y0 = foldr min y (cy ++ yy)
        y1 = foldr max y (cy ++ yy)

-- | @'render_controlwire_float' /x/ /ys/ /y/ /c/@: Render the line
-- connecting all control dots of the given controls, as well as a
-- floating \"global phase\" gate located just below (/x/, /y/). 
-- 
-- Parameters: /x/ is the current /x/-coordinate, /ys/ is an indexed
-- array of /y/-coordinates, /y/ is the /y/-coordinate of the wire
-- where the floating gate is attached, and /c/ is a list of controls.
render_controlwire_float :: X -> Map Wire Y -> Y -> Controls -> Draw ()
render_controlwire_float x ys y c = render_line x y0 x y1 
  where
    y' = y - 0.5
    cy = map (\(Signed w _) -> ys Map.! w) c
    y0 = minimum (y':cy)
    y1 = maximum (y':cy)

-- | @'render_controldots' /x/ /ys/ /c/@: Render the control dots
-- for the given controls.
render_controldots :: FormatStyle -> X -> Map Wire Y -> Controls -> Draw ()
render_controldots fs x ys c = do
  sequence_ [ renderdot x | x <- c ]
  where
    renderdot (Signed w True) = render_dot fs x (ys Map.! w)
    renderdot (Signed w False) = render_circle fs x (ys Map.! w)

-- | @'render_multi_gate' /x/ /ys/ /name/ /inv/ /wires/@: Render the
-- boxes for an /n/-ary gate of the given /name/, potentially
-- /inv/erted, at the given list of /wires/. The first two arguments
-- are the current /x/-coordinate and an indexed array of
-- /y/-coordinates.
render_multi_gate :: FormatStyle -> X -> Map Wire Y -> String -> InverseFlag -> [Wire] -> Draw ()
render_multi_gate fs x ys name inv [w] = 
  render_namedgate fs name inv x (ys Map.! w)
render_multi_gate fs x ys name inv ws =
  sequence_ [ render_namedgate fs (name ++ " " ++ show i) inv x (ys Map.! a) | (a,i) <- zip ws [1..] ]

-- | @'render_multi_named_ctrl' /x/ /ys/ /wires/ /names/@: Render
-- the boxes for multiple generalized controls at the given /wires/,
-- using the given /names/. We take special care of the fact that
-- generalized controls may be used non-linearly. 
render_multi_named_ctrl :: FormatStyle -> X -> Map Wire Y -> [Wire] -> [String] -> Draw ()
render_multi_named_ctrl fs x ys ws names =
  sequence_ [ render_circgate fs name x (ys Map.! a) | (a,name) <- IntMap.toList map ]
  where
    -- Combine the labels for w if w has multiple occurrences.
    map = IntMap.fromListWith (\x y -> y ++ "," ++ x) (zip ws names)

-- | @'render_multi_genctrl' /x/ /ys/ /wires/@: Render the boxes for
-- multiple (numbered) generalized controls at the given /wires/.
render_multi_genctrl :: FormatStyle -> X -> Map Wire Y -> [Wire] -> Draw ()
render_multi_genctrl fs x ys ws = render_multi_named_ctrl fs x ys ws names
  where
    names = map show [1..]
            
-- | Number a list of wires in increasing order, at the given
-- /x/-coordinate. If the boolean argument is 'True', put the numbers
-- to the right of /x/, else to the left.
render_ordering :: FormatStyle -> X -> Map Wire Y -> Bool -> [Wire] -> Draw ()
render_ordering fs x ys b ws =
  sequence_ [ render_number fs i b x (ys Map.! w) | (w,i) <- numbering ]
  where
    numbering = zip ws [1..]

-- | Render gate /g/ at /x/-coordinate /x/ and /y/-coordinates as
-- given by /ys/, which is a map from wires to
-- /y/-coordinates. Returns a pair (/s/,/t/) of draw actions for
-- background and foreground, respectively.
render_gate :: FormatStyle -> Gate -> X -> Map Wire Y -> Y -> (Draw (), Draw ())
render_gate fs g x ys maxh =
  let ymap w = ys Map.! w 
  in
  case g of
    -- Certain named gates are recognized for custom rendering.
    QGate "not" _ [w] [] c ncf -> (s2, t2 >> t3)
      where
        y = ymap w
        s2 = render_controlwire x ys [w] c
        t2 = render_controldots fs x ys c
        t3 = (render_not fs x y)
    QGate "multinot" _ ws [] c ncf -> (s2, t2 >> t3)
      where
        s2 = render_controlwire x ys ws c
        t2 = render_controldots fs x ys c
        t3 = sequence_ (map (\w -> (render_not fs x (ymap w))) ws)
    QGate "swap" _ [w1,w2] [] c ncf -> (s2, t2 >> t3)
      where
        y1 = ymap w1
        y2 = ymap w2
        s2 = render_controlwire x ys [w1,w2] c
        t2 = render_controldots fs x ys c
        t3 = (render_swap fs x y1) >> (render_swap fs x y2)
    QGate "trace" _ _ _ _ _ -> (return (), return ())
    QGate name inv ws1 ws2 c ncf -> (s2, t2 >> t3 >> t4)
      where
       s2 = render_controlwire x ys (ws1 ++ ws2) c
       t2 = render_multi_gate fs x ys name inv' ws1
       t3 = render_controldots fs x ys c
       t4 = render_multi_genctrl fs x ys ws2
       inv' = inv && not (self_inverse name ws1 ws2)
    QRot name inv theta ws1 ws2 c ncf -> (s2, t2 >> t3 >> t4)
      where
       s2 = render_controlwire x ys (ws1 ++ ws2) c
       t2 = render_multi_gate fs x ys name' inv ws1
       t3 = render_controldots fs x ys c
       t4 = render_multi_genctrl fs x ys ws2
       name' = substitute name '%' (dshow theta)
    GPhase t ws c ncf -> (s2, t2 >> t3)
      where
        y = case (ws, c) of
          ([], []) -> maximum (0.0 : Map.elems ys)
          ([], c)  -> minimum [ ymap w | Signed w b <- c ]
          (ws, c)  -> minimum [ ymap w | w <- ws ]
        s2 = render_controlwire_float x ys y c
        t2 = render_controldots fs x ys c
        t3 = (render_gphasegate fs (dshow t) x y)
    CNot w c ncf -> (s2, t2 >> t3)
      where
        y = ymap w
        s2 = render_controlwire x ys [w] c
        t2 = render_controldots fs x ys c
        t3 = (render_not fs x y)
    CGate "if" w [a,b,c] ncf -> (s2, t1 >> t3)  -- special case
      where
       y = ymap w
       s2 = render_controlwire x ys [w,a,b,c] []
       t1 = render_multi_named_ctrl fs x ys [a,b,c] ["if", "then", "else"]
       t3 = render_namedgate fs ">" False x y
    CGateInv "if" w [a,b,c] ncf -> (s2, t1 >> t3)  -- special case
      where
       y = ymap w
       s2 = render_controlwire x ys [w,a,b,c] []
       t1 = render_multi_named_ctrl fs x ys [a,b,c] ["if", "then", "else"]
       t3 = render_namedgate fs "<" False x y
    CGate name w c ncf -> (s2, t2 >> t3)
      where
       y = ymap w
       s2 = render_controlwire x ys (w:c) []
       t2 = render_multi_named_ctrl fs x ys c [ "  " | a <- c ]
       t3 = render_namedgate fs name False x y
    CGateInv name w c ncf -> (s2, t2 >> t3)
      where
       y = ymap w
       s2 = render_controlwire x ys (w:c) []
       t2 = render_multi_named_ctrl fs x ys c [ "  " | a <- c ]
       t3 = render_namedgate fs name True x y
    CSwap w1 w2 c ncf -> (s2, t2 >> t3)
      where
        y1 = ymap w1
        y2 = ymap w2
        s2 = render_controlwire x ys [w1,w2] c
        t2 = render_controldots fs x ys c
        t3 = (render_swap fs x y1) >> (render_swap fs x y2)
    QPrep w ncf -> (return (), t3)
      where
        y = ymap w
        t3 = (render_namedgate fs "prep" False x y)
    QUnprep w ncf -> (return (), t3)
      where
        y = ymap w
        t3 = (render_namedgate fs "unprep" False x y)
    QInit b w ncf -> (return (), t3)
      where
        y = ymap w
        t3 = (render_init fs (if b then "1" else "0") x y)
    CInit b w ncf -> (return (), t3)
      where
        y = ymap w
        t3 = (render_init fs (if b then "1" else "0") x y)
    QTerm b w ncf -> (return (), t3)
      where
        y = ymap w
        t3 = (render_term fs (if b then "1" else "0") x y)
    CTerm b w ncf -> (return (), t3)
      where
        y = ymap w
        t3 = (render_term fs (if b then "1" else "0") x y)
    QMeas w -> (return (), t3)
      where
        y = ymap w
        t3 = (render_namedgate fs "meas" False x y)
    QDiscard w -> (return (), t3)
      where
        y = ymap w
        t3 = (render_bar fs x y)
    CDiscard w -> (return (), t3)
      where
        y = ymap w
        t3 = (render_bar fs x y)
    DTerm b w -> (return (), t3)
      where
        y = ymap w
        t3 = (render_dterm fs (if b then "1" else "0") x y)
    Subroutine boxid inv ws1 a1 ws2 a2 c ncf scf rep -> (s2, t2 >> t3)
      where
       ws = union ws1 ws2
       s2 = render_controlwire x ys ws c
       t2 = render_multi_gate fs x ys label inv ws
       t3 = render_controldots fs x ys c
       show_rep = if rep == RepeatFlag 1 then "" else "(x" ++ show rep ++ ")"
       BoxId name shape = boxid
       label = name ++ show_rep ++ if (subroutineshape fs) then (", shape " ++ shape) else ""
    Comment s inv ws -> (return (), t1 >> t2)
      where
        t1 = render_comment fs (null ws) s' x (ymap 0) maxh
        t2 = sequence_ [render_label fs (null s) l x (ymap w) | (w,l) <- ws]
        s' = s ++ optional inv "*"

-- | Render the gates in the circuit. The parameters are: /xarity/:
-- the 'Xarity' of the currently pending wires. /xgs/: the list of
-- gates, paired with pre-computed /x/-coordinates. /ys/: a map from
-- wires to pre-computed /y/-coordinates. /x/: the right-most
-- /x/-coordinate where the final wires will be drawn to. /maxh/: the
-- maximal height of comments.
render_gates :: FormatStyle -> Xarity -> [(Gate, X)] -> Map Wire Y -> X -> Y -> (Draw (), Draw ())
render_gates fs xarity xgs ys x maxh =
  case xgs of
    [] ->
      let s2 = render_xarity fs ys xarity x
      in (s2, return ())
    (g,newx):gls ->
      let (xarity_term, xarity_new) = update_xarity xarity g newx in
      let s1 = render_xarity fs ys xarity_term newx in
      let (s2, t2) = render_gate fs g newx ys maxh in
      let (sx, tx) = render_gates fs xarity_new gls ys x maxh in
      (s1 >> s2 >> sx, t2 >> tx)

-- | PostScript definitions of various parameters.
ps_parameters :: FormatStyle -> String
ps_parameters fs =
  "% some parameters\n"
  ++ printf "%f setlinewidth\n" (linewidth fs)
  ++ printf "/gatepad %f def\n" (gatepad fs)
  ++ printf "/gateheight %f def\n" (gateheight fs)
  ++ printf "/stringbase %f def\n" (stringbase fs)
  ++ printf "/dotradius %f def\n" (dotradius fs)
  ++ printf "/oplusradius %f def\n" (oplusradius fs)
  ++ printf "/crossradius %f def\n" (crossradius fs)
  ++ printf "/barwidth %f def\n" (barwidth fs)
  ++ printf "/barheight %f def\n" (barheight fs)
  ++ printf "/dwidth %f def\n" (dwidth fs)
  ++ printf "/dheight %f def\n" (dheight fs)
  ++ printf "/maxgatelabelwidth %f def\n" (maxgatelabelwidth fs)
  ++ printf "/maxlabelwidth %f def\n" (maxlabelwidth fs)
  ++ printf "/maxnumberwidth %f def\n" (maxnumberwidth fs)
  ++ "/gatefont { /Times-Roman findfont .5 scalefont setfont } def\n"
  ++ "/labelfont { /Times-Roman findfont .3 scalefont setfont } def\n"
  ++ "/commentfont { /Times-Roman findfont .3 scalefont setfont } def\n"
  ++ "/numberfont { /Times-Roman findfont .5 scalefont setfont } def\n"
  ++ "/labelcolor { 0 0 1 setrgbcolor } def\n"
  ++ "/commentcolor { 1 0.2 0.2 setrgbcolor } def\n"
  ++ "/numbercolor { 0 0.7 0 setrgbcolor } def\n"

-- | PostScript definitions for various drawing subroutines. The
-- subroutines provided are:
-- 
-- > x0 y0 x1 y1 line       : draw a line from (x0,y0) to (x1,y1)
-- > x0 y0 x1 y1 dashedline : draw a dashed line from (x0,y0) to (x1,y1)
-- > x y h w rect           : draw a rectangle of dimensions w x h centered at (x,y)
-- > x y h w oval           : draw an oval of dimensions w x h centered at (x,y)
-- > x y dot           : draw a filled dot at (x,y)
-- > x y circ          : draw an empty dot at (x,y)
-- > x y oplus         : draw a "not" gate at (x,y)
-- > x y cross         : draw a cross ("swap" gate component) at (x,y)
-- > x y bar           : draw an init/term bar at (x,y)
-- > x y dbar          : draw a dterm bar at (x,y)
-- > name x y box      : draw an empty box at (x,y), big enough to fit name
-- > name x y gate     : draw a named box at (x,y)
-- > name x y circgate : draw a named round box at (x,y)
-- > name x y gphase   : draw a global phase gate (x,y)
-- > b x y init        : draw an "init" gate at (x,y), with state b
-- > b x y term        : draw a "term" gate at (x,y), with state b
-- > b x y dterm       : draw a "dterm" gate at (x,y), with state b
-- > string x y m b comment : draw a vertical comment at (x,y), with max height m and baseline adjustment b
-- > string x y clabel      : draw a wire label at (x,y), x-centered
-- > string x y rlabel      : draw a wire label at (x,y), right of x
-- > string x y lnumber     : draw a numbered input at (x,y)
-- > string x y rnumber     : draw a numbered output at (x,y)

ps_subroutines :: String
ps_subroutines = 
    "% subroutine definitions\n"
    ++ "/line { moveto lineto stroke } bind def\n"
    ++ "/dashedline { moveto gsave [0.3 0.2] .15 setdash lineto stroke grestore } bind def\n"
    ++ "/rect { /H exch def /W exch def -.5 W mul .5 H mul moveto W 0 rlineto 0 H neg rlineto W neg 0 rlineto closepath } bind def\n"
    ++ "/oval { /H exch def /W exch def gsave .5 W mul .5 H mul scale 0 0 1 0 360 newpath arc gsave 1.0 setgray fill grestore stroke grestore } bind def\n"
    ++ "/dot { dotradius 0 360 newpath arc gsave 0 setgray fill grestore newpath } bind def\n"
    ++ "/circ { dotradius 0 360 newpath arc gsave 1.0 setgray fill grestore stroke } bind def\n"
    ++ "/oplus { gsave translate 0 0 oplusradius 0 360 newpath arc gsave 1.0 setgray fill grestore stroke 0 oplusradius neg 0 oplusradius line oplusradius neg 0 oplusradius 0 line grestore } bind def\n"
    ++ "/cross { gsave translate crossradius dup dup neg dup line crossradius dup neg dup dup neg line grestore } bind def\n"
    ++ "/bar { gsave translate barwidth barheight rect fill grestore } bind def\n"
    ++ "/dbar { gsave translate barwidth 0.5 mul 0 translate dwidth dheight scale -1 -.5 moveto -.5 0 .5 -90 90 arc -1 .5 lineto closepath fill grestore } bind def\n"
    ++ "/box { gsave translate gatefont stringwidth pop /w exch def /w1 w gatepad add def w1 gateheight rect gsave 1.0 setgray fill grestore stroke grestore } bind def\n"
    ++ "/gate { gsave translate dup gatefont stringwidth pop /w exch def /fontscale w maxgatelabelwidth div def /fontscale fontscale 1 le {1} {fontscale} ifelse def /w2 w fontscale div def /w1 w2 gatepad add def w1 gateheight rect gsave 1.0 setgray fill grestore stroke 1 fontscale div dup scale 0 .5 w mul sub -0.5 stringbase mul moveto show grestore } bind def\n"
    ++ "/circgate  {gsave translate dup gatefont stringwidth pop /w exch def /fontscale w maxgatelabelwidth div def /fontscale fontscale 1 le {1} {fontscale} ifelse def /w2 w fontscale div def /w1 w2 gatepad add def w1 0.8 gateheight mul oval gsave 1.0 setgray fill grestore stroke 1 fontscale div dup scale 0 .5 w mul sub -0.5 stringbase mul moveto show grestore } bind def\n"
    ++ "/gphase { gsave translate 0 -0.5 circgate grestore } bind def\n"
    ++ "/init { gsave translate dup gatefont stringwidth pop /w exch def /w1 w gatepad add def -.5 w1 mul 0 translate 0.5 w1 mul 0 bar 0 .5 w mul sub -0.5 stringbase mul moveto show grestore } bind def\n"
    ++ "/term { gsave translate dup gatefont stringwidth pop /w exch def /w1 w gatepad add def .5 w1 mul 0 translate -0.5 w1 mul 0 bar 0 .5 w mul sub -0.5 stringbase mul moveto show grestore } bind def\n"
    ++ "/dterm { gsave translate dup gatefont stringwidth pop /w exch def /w1 w gatepad add def .5 w1 mul 0 translate -0.5 w1 mul 0 dbar 0 .5 w mul sub -0.5 stringbase mul moveto show grestore } bind def\n"
    ++ "/comment { gsave /b exch def /maxh exch def /y exch def /x exch def commentfont commentcolor x y maxh sub x y 0.4 add 1.0 b textbox grestore } bind def\n"
    ++ "/clabel { gsave translate dup labelfont stringwidth pop /w exch def /fontscale w maxlabelwidth 2 mul div def /fontscale fontscale 1 le {1} {fontscale} ifelse def 0 0.15 translate 1 fontscale div dup scale -0.5 w mul 0 moveto labelcolor show grestore } bind def\n"
    ++ "/rlabel { gsave translate dup labelfont stringwidth pop /w exch def /fontscale w maxlabelwidth div def /fontscale fontscale 1 le {1} {fontscale} ifelse def 0 0.15 translate 1 fontscale div dup scale 0 0 moveto labelcolor show grestore } bind def\n"
    ++ "/lnumber { gsave translate dup numberfont stringwidth pop /w exch def /fontscale w maxnumberwidth div def /fontscale fontscale 1 le {1} {fontscale} ifelse def -0.2 -0.15 translate 1 fontscale div dup scale -1 w mul 0 moveto numbercolor show grestore } bind def\n"
    ++ "/rnumber { gsave translate dup numberfont stringwidth pop /w exch def /fontscale w maxnumberwidth div def /fontscale fontscale 1 le {1} {fontscale} ifelse def 0.2 -0.15 translate 1 fontscale div dup scale 0 0 moveto numbercolor show grestore } bind def\n"
      
-- | @'page_of_ocircuit' name ocirc@: Render the circuit /ocirc/ on a
-- single page.
-- 
-- The rendering takes place in the following user coordinate system:
-- 
-- \[image coord.png]
page_of_ocircuit :: FormatStyle -> Maybe BoxId -> OCircuit -> Document ()
page_of_ocircuit fs boxid ocirc = do
  newpage bboxx bboxy $ do
    when (isJust boxid) $ do
      comment ("drawing commands for " ++ string_of_boxid (fromJust boxid))
    
    -- set up the user coordinate system
    scale sc sc
    translate ((xoff fs) + 1) 1
    
    -- drawing commands
    setlinewidth (linewidth fs)
    when (isJust boxid) $ do
      textbox align_left (gatefont fs) (foregroundcolor fs) (-(xoff fs)) (raw_height-0.25) raw_width (raw_height-0.25) (stringbase fs) ("Subroutine " ++ string_of_boxid (fromJust boxid) ++ ":")
    rendered_wires
    rendered_gates
    render_ordering fs (-(xoff fs)) ys False w_in
    render_ordering fs raw_width ys True w_out
  where
    -- unit scale: distance, in points, between wires
    sc = 10
    
    -- decompose OCircuit
    OCircuit (w_in, circ, w_out) = ocirc
    (a1,gs,a2,_) = circ
    
    -- figure out y-coordinates and height
    ws = wirelist_of_circuit circ
    raw_height = fromIntegral $ length ws
    ys = Map.fromList (zip (reverse ws) [0.0 ..])
    maxh = raw_height + 0.3
    bboxy = sc * (raw_height + 1)
    
    -- figure out x-coordinates and width
    (raw_width,xgs) = assign_x_coordinates fs gs 0.0
    bboxx = sc * (raw_width + (xoff fs) + 2.0)
    
    xa1 = IntMap.map (\t -> (t, -(xoff fs))) a1
    (rendered_wires, rendered_gates) = render_gates fs (Map.fromList (IntMap.assocs xa1)) xgs ys raw_width maxh

-- | Render a low-level boxed quantum circuit as a graphical
-- 'Document'. If there are subroutines, each of them is placed on a
-- separate page.
render_bcircuit :: FormatStyle -> BCircuit -> Document ()
render_bcircuit fs (circ, namespace) = do
  page_of_ocircuit fs Nothing (OCircuit ([], circ, []))
  sequence_ [ page_of_ocircuit fs (Just boxid) ocirc | (boxid, TypedSubroutine ocirc _ _ _) <- Map.toList namespace]

-- | Render a low-level dynamic quantum circuit as a graphical
-- 'Document'. If there are subroutines, each of them is placed on a
-- separate page.  If the circuit uses dynamic lifting, an error is
-- produced.
render_dbcircuit :: FormatStyle -> ErrMsg -> DBCircuit a -> Document ()
render_dbcircuit fs e dbcirc = render_bcircuit fs bcirc where
  (bcirc, _) = bcircuit_of_static_dbcircuit errmsg dbcirc
  errmsg x = e ("operation not permitted during graphical rendering: " ++ x)

-- | Print a representation of a low-level quantum circuit, in the
-- requested graphics format, directly to standard output. If there
-- are boxed subcircuits, each of them is placed on a separate page.
print_bcircuit_format :: FormatStyle -> BCircuit -> IO ()
print_bcircuit_format fs bcirc =
  render_custom_stdout (renderformat fs) cust (render_bcircuit fs bcirc)
    where
      cust = custom {
        creator = "Quipper",
        ps_defs = ps_parameters fs ++ ps_subroutines 
        }

-- | Print a representation of a low-level dynamic quantum circuit, in
-- the requested graphics format, directly to standard output. If
-- there are boxed subcircuits, each of them is placed on a separate
-- page. If the circuit uses dynamic lifting, an error is produced.
print_dbcircuit_format :: FormatStyle -> ErrMsg -> DBCircuit a -> IO ()
print_dbcircuit_format fs e dbcirc = 
  render_custom_stdout (renderformat fs) cust (render_dbcircuit fs e dbcirc)
    where 
      cust = custom {
        creator = "Quipper",
        ps_defs = ps_parameters fs ++ ps_subroutines
        }

-- ----------------------------------------------------------------------
-- * Previewing

-- | Display a document directly in Acrobat Reader. This may not be
-- portable. It requires the external program \"acroread\" to be
-- installed.
preview_document :: Document a -> IO a
preview_document = preview_document_custom custom

-- | Display a document directly in Acrobat Reader. This may not be
-- portable. It requires the external program \"acroread\" to be
-- installed.
preview_document_custom :: Custom -> Document a -> IO a
preview_document_custom custom doc = do
  tmpdir <- getTemporaryDirectory
  (pdffile, fd) <- openTempFile tmpdir "Quipper.pdf"
  a <- render_custom_file fd Format_PDF custom doc
  hClose fd
  system_pdf_viewer 100 pdffile
  removeFile pdffile
  return a

-- | Display the circuit directly in Acrobat Reader. This may not be
-- portable. It requires the external program \"acroread\" to be
-- installed.
preview_bcircuit :: BCircuit -> IO ()
preview_bcircuit bcirc =
  preview_document doc
  where
    doc = render_bcircuit pdf bcirc

-- | Display a low-level dynamic quantum circuit directly in Acrobat
-- Reader. This may not be portable. It requires the external program
-- \"acroread\" to be installed. If the circuit uses dynamic lifting,
-- an error is produced.
preview_dbcircuit :: ErrMsg -> DBCircuit a -> IO ()
preview_dbcircuit e dbcirc = preview_bcircuit bcirc where
  (bcirc, _) = bcircuit_of_static_dbcircuit errmsg dbcirc
  errmsg x = e ("operation not permitted for PDF preview: " ++ x)

-- ----------------------------------------------------------------------
-- * Gate counts

-- ** Gate types

-- $ The type 'Gate' contains too much information to be used as the 
-- index for counting gates: all 'CNot' gates, for instance,
-- should be counted together, regardless of what wires they are
-- applied to.
--
-- We define 'Gatetype' to remedy this, with each value of 
-- 'Gatetype' corresponding to an equivalence class of
-- gates as they should appear in gate counts.
--
-- During gate counting, a little more information needs to be retained,
-- so that operations such as adding controls to subroutine counts can
-- be accurately performed.  'AnnGatetype' supplies this information.

-- | An abbreviated representation of the controls of a gate: 
-- the number of positive and negative controls, respectively.
type ControlType = (Int,Int) 

-- | From a list of controls, extract the number of positive and
-- negative controls.
controltype :: Controls -> ControlType
controltype c =
  (length $ filter get_sign c, length $ filter (not . get_sign) c)

-- | Convenience constant for uncontrolled gates.
nocontrols :: ControlType
nocontrols = (0,0)

-- | A data type representing equivalence classes of basic gates,
-- for the output of gatecounts.
data Gatetype = 
  Gatetype String ControlType
  | GatetypeSubroutine BoxId InverseFlag ControlType
 deriving (Eq, Ord, Show)

-- | A data type analogous to 'Gatetype', but with extra annotations,
-- e.g. a 'NoControlFlag', for use in the computation of gate counts.
data AnnGatetype = 
    AnnGatetype String (Maybe String) ControlType NoControlFlag ControllableFlag
  | AnnGatetypeSubroutine BoxId InverseFlag ControlType NoControlFlag ControllableFlag
  deriving (Eq, Ord, Show)

-- | Forget the annotations of an 'AnnGatetype'
unannotate_gatetype :: AnnGatetype -> Gatetype
unannotate_gatetype (AnnGatetype n _ cs _ _) = Gatetype n cs
unannotate_gatetype (AnnGatetypeSubroutine n i cs _ _) = GatetypeSubroutine n i cs

-- | Add controls to an annotated gate type, or throw an error message if it is not controllable; 
-- unless its 'NoControlFlag' is set, in which case leave it unchanged.
add_controls_gatetype :: ErrMsg -> ControlType -> AnnGatetype -> AnnGatetype
add_controls_gatetype e (x',y') g@(AnnGatetype n n_inv (x,y) ncf cf) =
  if ncf then g
  else case cf of
     AllCtl           -> AnnGatetype n n_inv (x+x',y+y') ncf cf
     OnlyClassicalCtl -> AnnGatetype n n_inv (x+x',y+y') ncf cf
     NoCtl            -> error $ e "add_controls_gatetype: gate " ++ n ++ " is not controllable."

add_controls_gatetype e (x',y') g@(AnnGatetypeSubroutine n inv (x,y) ncf cf) =
  if ncf then g
  else case cf of
     AllCtl           -> AnnGatetypeSubroutine n inv (x+x',y+y') ncf cf
     OnlyClassicalCtl -> AnnGatetypeSubroutine n inv (x+x',y+y') ncf cf
     NoCtl            -> error $ e "add_controls_gatetype: subroutine " ++ show n ++ " is not controllable."

-- | Reverse an annotated gate type, of throw an error if it is not reversible. 
reverse_gatetype :: ErrMsg -> AnnGatetype -> AnnGatetype
reverse_gatetype e g@(AnnGatetype n n_inv cs ncf cf) =
  case n_inv of
    Just n' -> (AnnGatetype n' (Just n) cs ncf cf)
    Nothing -> error $ e "reverse_gatetype: gate " ++ n ++ " is not reversible"
reverse_gatetype e g@(AnnGatetypeSubroutine n inv cs ncf cf) =
  (AnnGatetypeSubroutine n (not inv) cs ncf cf)

-- | Set the 'NoControlFlag' of an annotated gate type to 'True'.
set_ncf_gatetype :: AnnGatetype -> AnnGatetype
set_ncf_gatetype (AnnGatetype n n_inv cs ncf cf) =
                 (AnnGatetype n n_inv cs True cf)
set_ncf_gatetype (AnnGatetypeSubroutine n inv cs ncf cf) =
                 (AnnGatetypeSubroutine n inv cs True cf)

-- | Helper function for 'gatetype': append a formatted arity to a string.
with_arity :: String -> Int -> String
n `with_arity` a = n ++ ", arity " ++ show a

-- | Convert a given low-level gate to an annotated gate type
gatetype :: Gate -> AnnGatetype
gatetype (QGate n inv ws vs c ncf) =
  AnnGatetype (n' inv') (Just $ n' $ notinv') (controltype c) ncf AllCtl
  where 
    n' b = (n ++ optional b "*") `with_arity` (length ws + length vs)
    inv' = inv && not (self_inverse n ws vs)
    notinv' = not inv && not (self_inverse n ws vs)
gatetype (QRot n inv t ws vs c ncf) =
  AnnGatetype (n' inv) (Just $ n' $ not inv) (controltype c) ncf AllCtl
  where n' b = (printf "Rot(%s,%f)" (n++ optional b "*") t) `with_arity` (length ws + length vs)
gatetype (GPhase t w c ncf) = 
  AnnGatetype (phase_name t) (Just $ phase_name (-t)) (controltype c) ncf AllCtl
  where phase_name t = (printf "exp^(%f i pi)" t)
gatetype (CNot w c ncf) = 
  AnnGatetype "CNot" (Just "CNot") (controltype c) ncf AllCtl
gatetype (CGate n w ws ncf) = 
  AnnGatetype (n' True) (Just $ n' False) nocontrols ncf AllCtl
  where n' b = n ++ optional b "*" `with_arity` length ws
gatetype (CGateInv n w ws ncf) =
  AnnGatetype (n' False) (Just $ n' True) nocontrols ncf AllCtl
  where n' b = n ++ optional b "*" `with_arity` length ws
gatetype (CSwap w v c ncf) =
  AnnGatetype "CSwap" (Just "CSwap") (controltype c) ncf AllCtl
gatetype (QPrep w ncf) =
  AnnGatetype "Prep" (Just "Unprep") nocontrols ncf NoCtl
gatetype (QUnprep w ncf) = 
  AnnGatetype "Unprep" (Just "Prep") nocontrols ncf NoCtl
gatetype (QInit b w ncf) =
  AnnGatetype ("Init" ++ b') (Just $ "Term" ++ b') nocontrols ncf NoCtl
  where b' = show $ if b then 1 else 0
gatetype (CInit b w ncf) =
  AnnGatetype ("CInit" ++ b') (Just $ "CTerm" ++ b') nocontrols ncf NoCtl
  where b' = show $ if b then 1 else 0
gatetype (QTerm b w ncf) =
  AnnGatetype ("Term" ++ b') (Just $ "Init" ++ b') nocontrols ncf NoCtl
  where b' = show $ if b then 1 else 0
gatetype (CTerm b w ncf) =
  AnnGatetype ("CTerm" ++ b') (Just $ "CInit" ++ b') nocontrols ncf NoCtl
  where b' = show $ if b then 1 else 0
gatetype (QMeas w) = 
  AnnGatetype "Meas" Nothing nocontrols False NoCtl
gatetype (QDiscard w) =
  AnnGatetype "Discard" Nothing nocontrols False NoCtl
gatetype (CDiscard w) =
  AnnGatetype "CDiscard" Nothing nocontrols False NoCtl
gatetype (DTerm b w) =
  AnnGatetype "CDiscard" Nothing nocontrols False NoCtl
gatetype (Subroutine boxid inv ws1 a1 ws2 a2 c ncf ctrble reps) =
  AnnGatetypeSubroutine boxid inv (controltype c) ncf ctrble
gatetype (Comment _ inv ws) = AnnGatetype ("Comment") (Just "Comment") nocontrols True NoCtl

-- | Convert a gate type to a human-readable string.
string_of_gatetype :: Gatetype -> String
string_of_gatetype (Gatetype s (c1,c2)) =
  printf "\"%s\"" s
  ++ if c2==0 && c1==0 then "" else
     if c2==0 then printf ", controls %d" c1 else
     printf " controls %d+%d" c1 c2
string_of_gatetype (GatetypeSubroutine boxid i (c1,c2)) =
  "Subroutine" ++ optional i "*" ++ cs ++ ": " ++ string_of_boxid boxid
  where
    cs = if c2==0 && c1==0 then "" else
         if c2==0 then printf ", controls %d" c1 else
         printf " controls %d+%d" c1 c2

-- ** Gate counts

-- | Gate counts of circuits.  
type Gatecount = Map Gatetype Integer

-- | Annotated gate counts of circuits.  
type AnnGatecount = Map AnnGatetype Integer

-- | Given the (annotated) gatecount of a circuit, return the gatecount of
-- the reverse circuit, or throw an error if any component is not reversible.
reverse_gatecount :: ErrMsg -> AnnGatecount -> AnnGatecount
reverse_gatecount e = Map.mapKeysWith (+) (reverse_gatetype e)

-- | Given the (annotated) gatecount of a circuit, return the gatecount of
-- the same circuit with controls added, or throw an error if any component
-- is not controllable.
add_controls_gatecount :: ErrMsg -> ControlType -> AnnGatecount -> AnnGatecount
add_controls_gatecount e cs = Map.mapKeysWith (+) (add_controls_gatetype e cs)

-- | Set the ncf of all gates in a gatecount to 'True'.
set_ncf_gatecount :: AnnGatecount -> AnnGatecount
set_ncf_gatecount = Map.mapKeysWith (+) set_ncf_gatetype

-- | Remove the annotations from a gatecount.
unannotate_gatecount :: AnnGatecount -> Gatecount
unannotate_gatecount = Map.mapKeysWith (+) unannotate_gatetype

-- | Input a list of items and output a map from items to counts.
-- Example: 
-- 
-- > count ['a', 'b', 'a'] = Map.fromList [('a',2), ('b',1)]
count :: (Ord a, Num int) => [(int,a)] -> Map a int
count list =
  foldl' (\mp (i,x) -> MapS.insertWith (+) x i mp) Map.empty list 

-- | Count the number of gates of each type in a circuit, with annotations,
-- treating subroutine calls as atomic gates.
anngatecount_of_circuit :: Circuit -> AnnGatecount
anngatecount_of_circuit (_,gs,_,_) = count $ map (\x -> (repeated x, gatetype x)) $ filter (not . is_comment) gs
  where
    is_comment (Comment _ _ _) = True
    is_comment _ = False
    repeated (Subroutine _ _ _ _ _ _ _ _ _ (RepeatFlag repeat)) = repeat
    repeated _ = 1

-- | Count the number of gates of each type in a circuit,
-- treating subroutine calls as atomic gates.
gatecount_of_circuit :: Circuit -> Gatecount
gatecount_of_circuit = unannotate_gatecount . anngatecount_of_circuit

-- | Given an 'AnnGatetype' describing a subroutine call
-- (possibly repeated),
-- and a gate count for the subroutine itself, return the gatecount 
-- of the subroutine call.
--
-- (This may be the reverse of the original subroutine, may have
-- controls added, etc.)
gatecount_of_subroutine_call :: ErrMsg -> AnnGatetype -> RepeatFlag -> AnnGatecount -> AnnGatecount
gatecount_of_subroutine_call e (AnnGatetypeSubroutine boxid inv cs ncf ctrble) (RepeatFlag reps) =
  (if inv then reverse_gatecount err_inv else id)
  . (if cs == nocontrols then id
       else case ctrble of
             AllCtl           -> add_controls_gatecount err_ctrl cs
             OnlyClassicalCtl -> add_controls_gatecount err_ctrl cs
             NoCtl            -> error $ err_ctrble)
  . (if reps == 1 then id else (Map.map (* reps)))
  . (if ncf then set_ncf_gatecount else id) 
  where
    err_inv = e . (("gatecount_of_subroutine_call, inverting subroutine " ++ longname ++ ": ") ++)
    err_ctrl = e . (("gatecount_of_subroutine_call, controlling subroutine " ++ longname ++ ": ") ++)
    err_ctrble = e $ "gatecount_of_subroutine_call: subroutine " ++ longname ++ " not controllable"
    longname = string_of_boxid boxid
    
gatecount_of_subroutine_call e _ _ = error $ e "internal error (gatecount_of_subroutine_call called on non-subroutine)"

-- | Given a circuit and gatecounts for its subroutines, 
-- give an (aggregated) gatecount for the circuit.
anngatecount_of_circuit_with_sub_cts :: ErrMsg -> Map BoxId AnnGatecount -> Circuit -> AnnGatecount
anngatecount_of_circuit_with_sub_cts e sub_cts (_,gs,_,_) =
  foldr action Map.empty gs
  where
    action (Comment _ _ _) = id
    action g@(Subroutine n _ _ _ _ _ _ _ _ reps) = 
      case Map.lookup n sub_cts of
        Nothing -> error $ e $ "subroutine not found: " ++ show n
        Just n_ct -> flip (Map.unionWith (+)) $
                       gatecount_of_subroutine_call e (gatetype g) reps n_ct
    action g = MapS.insertWith (+) (gatetype g) 1

-- | Give the aggregate gate count of a 'BCircuit'; that is, the
-- the total count of basic gates once all subroutines are fully inlined.
aggregate_gatecounts_of_bcircuit :: BCircuit -> Gatecount
aggregate_gatecounts_of_bcircuit (main_circ, namespace)
  = unannotate_gatecount $
    anngatecount_of_circuit_with_sub_cts e sub_cts main_circ
    where
      sub_cts = Map.map (anngatecount_of_circuit_with_sub_cts e sub_cts . circuit_of_typedsubroutine) namespace
      e = ("aggregate_gatecounts_of_bcircuit: " ++)

-- ** Wire usage count

-- | Count by how much a low-level gate changes the number of wires in the arity.

-- Implementation note: writing this function explicitly case-by-case appears
-- very slightly faster (~0.5%), but more fragile/less maintainable.
gate_wires_change :: Gate -> Integer
gate_wires_change g = 
  let (a_in,a_out) = gate_arity g
  in fromIntegral $ length a_out - length a_in

-- | Find the maximum number of wires used simultaneously in a 'BCircuit',
-- assuming all subroutines inlined. 
aggregate_maxwires_of_bcircuit :: BCircuit -> Integer
aggregate_maxwires_of_bcircuit (main_circ, namespace)
  = maxwires_of_circuit_with_sub_maxwires e sub_maxs main_circ
    where
      e = ("aggregate_maxwires_of_bcircuit: " ++)
      sub_maxs = Map.map (maxwires_of_circuit_with_sub_maxwires e sub_maxs . circuit_of_typedsubroutine) namespace

-- | Given a circuit and gatecounts for its subroutines, 
-- give an (aggregated) gatecount for the circuit.
maxwires_of_circuit_with_sub_maxwires :: ErrMsg -> Map BoxId Integer -> Circuit -> Integer
maxwires_of_circuit_with_sub_maxwires e sub_maxs (a1,gs,a2,_) =
  snd $ foldl (flip action) (in_wires, in_wires) gs
  where
    in_wires = fromIntegral $ IntMap.size a1
    update w_change (!w_old, !wmax_old) =
-- Implementation note: strictness in this pattern is to avoid putting the whole
-- tower of “max” applications on the stack.
      let w_new = w_old + w_change in (w_new, max wmax_old w_new)
    action g@(Subroutine n _ ws1 _ ws2 _ _ _ _ (RepeatFlag r)) = 
      case Map.lookup n sub_maxs of
        Nothing -> error $ "subroutine not found: " ++ show n
        Just n_max -> (update $ (fromIntegral $ length ws2) - n_max)
                      . (update $ n_max - (fromIntegral $ length ws1))
    action g = update $ gate_wires_change g

-- ** Printing gate counts

-- | Print a gate count, as a table of integers and gate types. 
print_gatecount :: Gatecount -> IO ()
print_gatecount cts = mapM_
  (\(gt,k) -> putStr (printf ("%" ++ show max_digits ++ "d: %s\n") k (string_of_gatetype gt)))
  (Map.assocs cts)
  where
    max_digits = maximum $ 5:(map ((1+) . floor . logBase 10 . fromIntegral) (Map.elems cts))

-- | Print the simple gate count, plus summary information, for a simple circuit.
print_gatecounts_circuit :: Circuit -> IO ()
print_gatecounts_circuit circ@(a1,gs,a2,n) = do
  print_gatecount cts
  putStrLn $ printf "Total gates: %d" $ sum $ Map.elems cts
  putStrLn $ printf "Inputs: %d" $ IntMap.size a1
  putStrLn $ printf "Outputs: %d" $ IntMap.size a2
  putStrLn $ printf "Qubits in circuit: %d" n
  where
    cts = gatecount_of_circuit circ

-- | Print gate counts for a boxed circuit:
-- first the simple gate count for each subroutine separately,
-- then the aggregated count for the whole circuit.
print_gatecounts_bcircuit :: BCircuit -> IO ()
print_gatecounts_bcircuit bcirc@(circ@(a1,_,a2,_),namespace) = do
  print_gatecounts_circuit circ
  when (not $ Map.null namespace) $ do
    sequence_ [ (putStrLn "") >> (print_gatecounts_subroutine sub) | sub <- Map.toList namespace ]
    putStrLn ""
    putStrLn "Aggregated gate count:" 
    let aggregate_cts = aggregate_gatecounts_of_bcircuit bcirc
        maxwires = aggregate_maxwires_of_bcircuit bcirc
    print_gatecount aggregate_cts
    putStrLn $ printf "Total gates: %d" $ sum $ Map.elems aggregate_cts
    putStrLn $ printf "Inputs: %d" $ IntMap.size a1
    putStrLn $ printf "Outputs: %d" $ IntMap.size a2
    putStrLn $ printf "Qubits in circuit: %d" maxwires

-- | Print gate counts for a named subroutine.
print_gatecounts_subroutine :: (BoxId, TypedSubroutine) -> IO ()
print_gatecounts_subroutine (boxid, TypedSubroutine ocirc _ _ _) = do
  putStrLn ("Subroutine: " ++ show name)
  putStrLn ("Shape: " ++ show shape)
  print_gatecounts_circuit circ
  where
    OCircuit (_, circ, _) = ocirc
    BoxId name shape = boxid

-- | Print gate counts for a static 'DBCircuit'. The circuit may not
-- use any dynamic lifting, or else an error will be produced.
print_gatecounts_dbcircuit :: ErrMsg -> DBCircuit a -> IO ()
print_gatecounts_dbcircuit e dbcirc = print_gatecounts_bcircuit bcirc where
  (bcirc, _) = bcircuit_of_static_dbcircuit errmsg dbcirc
  errmsg x = e ("operation not permitted during gate count: " ++ x)

-- ----------------------------------------------------------------------
-- * Printing to multiple formats

-- | Available output formats.

data Format = 
  EPS         -- ^ Encapsulated PostScript graphics.
  | PDF       -- ^ Portable Document Format. One circuit per page.
  | PS        -- ^ PostScript. One circuit per page.
  | ASCII     -- ^ A textual representation of circuits.
  | QASM     -- ^ A qasm textual representation of circuits.  
  | Preview   -- ^ Don't print anything, but preview directly on screen (requires the external program /acroread/).
  | GateCount -- ^ Print statistics on gate counts.
  | CustomStyle FormatStyle
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
  ("ascii", ASCII),  
  ("preview", Preview),
  ("gatecount", GateCount)
  ]
                    
-- | Print a low-level quantum circuit directly to the IO monad, using
-- the specified format.
print_dbcircuit :: Format -> ErrMsg -> DBCircuit a -> IO ()
print_dbcircuit EPS = print_dbcircuit_format eps
print_dbcircuit PDF = print_dbcircuit_format pdf
print_dbcircuit PS = print_dbcircuit_format ps
print_dbcircuit QASM = print_dbcircuit_qasm
print_dbcircuit Preview = preview_dbcircuit
print_dbcircuit GateCount = print_gatecounts_dbcircuit
print_dbcircuit (CustomStyle fs) = print_dbcircuit_format fs

-- | Print a document to the requested format, which must be one of
-- 'PS', 'PDF', 'EPS', or 'Preview'.
print_of_document :: Format -> Document a -> IO a
print_of_document = print_of_document_custom custom

-- | Like 'print_of_document', but also takes a 'Custom' data
-- structure.
print_of_document_custom :: Custom -> Format -> Document a -> IO a
print_of_document_custom custom PS doc = render_custom_stdout Format_PS custom doc
print_of_document_custom custom PDF doc = render_custom_stdout Format_PDF custom doc
print_of_document_custom custom EPS doc = render_custom_stdout (Format_EPS 1) custom doc
print_of_document_custom custom Preview doc = preview_document_custom custom doc
print_of_document_custom custom format doc = error ("print_of_document: method " ++ show format ++ " can't be used in this context")

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
