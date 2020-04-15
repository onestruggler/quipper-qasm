--module Quipper2QASM where

import Quipper.Libraries.QuipperASCIIParser
--import qualified GateStruct as MyGateS
import Quipper.Internal.Circuit
import Quipper.Internal.Printing
import Quipper.Utils.Auxiliary
import Quipper.Internal.Monad

import Quipper
import Data.List

--import Bmv5
--import GateStruct

import System.Environment
import Control.Monad


import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.IntSet as IS

test_gate = QGate "TT" False [0,1,2] [3,4] [Signed 5 True, Signed 6 False] True

t_arity_in = IM.fromList (map (\x -> (x , Qbit)) [0..10])
t_arity_out = IM.fromList (map (\x -> (x , Qbit)) [0..5])

cir = (t_arity_in, [test_gate], t_arity_out, (11::Int))

bcir = (cir, M.empty)

exta = xintmap_inserts (IM.toList t_arity_in) xintmap_empty

-- | QASM supported gate
data QASM_Gate  =   I Int
            | Init Int -- for recording which wire is in state |0>
            | Init' String Int -- added for .qc parser
            | H Int
            | X Int
            | Y Int
            | Cnot Int Int
            | CX Int Int
            | CCX Int Int Int            
            | Cnot' Int Int -- When control qubit is |0>, it fires           
            | T Int
            | T' Int
            | CS Int Int
            | CS' Int Int
            | CZ Int Int
            | Swap Int Int
            | CCZ Int Int Int
            | S Int
            | S' Int
            | Z Int
            | CNZ [Int]  -- Added for the new translation
            | Toffoli Int Int Int  -- standard toffoli
            | Toffoli0 Int Int Int  -- fires when controls is |00> 
            | Toffoli1 Int Int Int  -- fires when controls is |01> 
            | Toffoli2 Int Int Int  -- fires when controls is |10> 
            | Toffoli3 Int Int Int  -- fires when controls is |11> 
            | Toffoli4 Int Int Int Int
            | Toffolin [Int]
            | P [Int]
            | M Int
            | Term Int -- after Term0 i, the wire i is in state |0>
            | Term' String Int -- added for .qc parser
            | BDot -- added for matrix representation, black control
            | WDot -- added for matrix representation, white control
            | Nil -- added for matrix representation
            | Rx Int Double
            | Ry Int Double
            | Rz Int Double
            | X90 Int
            | Y90 Int
            | MX90 Int
            | MY90 Int
            | CRk Int Int Int
            | CR Int Int Double
            | BC Int QASM_Gate
            deriving (Eq,Ord,Read,Show)


qshow :: Int -> String
qshow i = "q[" ++ show i ++ "]"

gshow :: QASM_Gate -> String
gshow (I x) = "I " ++ qshow x
gshow (H x) = "H " ++ qshow x
gshow (X x) = "X " ++ qshow x
gshow (Y x) = "Y " ++ qshow x
gshow (Z x) = "Z " ++ qshow x
gshow (S x) = "S " ++ qshow x
gshow (S' x) = "Sdag " ++ qshow x
gshow (T x) = "T " ++ qshow x
gshow (T' x) = "Tdag " ++ qshow x
gshow (CX t c) = "CNOT " ++ intercalate ", " [qshow c, qshow t]
gshow (CZ t c) = "CZ " ++ intercalate ", " [qshow c, qshow t]
gshow (Swap t c) = "SWAP " ++ intercalate ", " [qshow c, qshow t]
gshow (Toffoli t c1 c2) = "Toffoli " ++ intercalate ", " [qshow c1, qshow c2, qshow t]
gshow (Rz x theta) = "Rz " ++ qshow x ++ ", " ++ show theta


ccz2ccx :: QASM_Gate -> [QASM_Gate]
ccz2ccx (CCZ t c1 c2) = [H t, Toffoli t c1 c2, H t]
ccz2ccx g = [g]

ccz2ccxs xs = concat $ map ccz2ccx xs


quipper_gate2my_gate :: Gate -> QASM_Gate
quipper_gate2my_gate (QGate "not" False w1 w2 ctrls True) =
  case length ctrls of
    0 -> X (head w1)     
    1 -> case get_sign (head ctrls) of
      True -> CX (head w1) (from_signed (head ctrls))
      False -> Cnot' (head w1) (from_signed (head ctrls))      
    2 -> case all get_sign ctrls of
        True -> Toffoli (head w1) (head (map from_signed ctrls)) (last (map from_signed ctrls))
    _ -> case all get_sign ctrls of
        True -> Toffolin ((head w1) : (map from_signed ctrls))
quipper_gate2my_gate (QGate "Z" False w1 w2 ctrls True) =
  case length ctrls  of
    0 -> Z (head w1) 
    1 -> case get_sign (head ctrls) of
      True -> CZ (head w1) (from_signed (head ctrls))
    2 -> case all get_sign ctrls of
      True -> CCZ (head w1) (head (map from_signed ctrls)) (last (map from_signed ctrls))
    _ -> case all get_sign ctrls of
      True -> CNZ ((head w1) : (map from_signed ctrls))
quipper_gate2my_gate (QGate "H" False w1 w2 ctrls True) =
  case length ctrls of
    0 -> H (head w1)
quipper_gate2my_gate (QGate "S" False w1 w2 ctrls True) =
  case length ctrls of
    0 -> S (head w1)
quipper_gate2my_gate (QGate "Sdag" False w1 w2 ctrls True) =
  case length ctrls of
    0 -> S' (head w1)
quipper_gate2my_gate (QGate "T" False w1 w2 ctrls True) =
  case length ctrls of
    0 -> T (head w1)
quipper_gate2my_gate (QGate "Tdag" False w1 w2 ctrls True) =
  case length ctrls of
    0 -> T' (head w1)
quipper_gate2my_gate (QGate "Swap" False w1 w2 ctrls True) =
  case length ctrls of
    0 -> Swap (head w1) (last w1)
quipper_gate2my_gate (QGate "I" False w1 w2 ctrls True) =
  case length ctrls of
    0 -> I (head w1)
quipper_gate2my_gate (QRot "exp(-i%Z)" False theta w1 w2 ctrls True) =
  case length ctrls of
    0 -> Rz (head w1) theta
quipper_gate2my_gate (QRot "exp(-i%Z)" False theta w1 w2 ctrls False) =
  case length ctrls of
    0 -> Rz (head w1) theta


main' :: String -> IO ()
main' file = do
  str <- readFile $ file
  let (eps , circ ) = parse_circuit str
  let len = length eps
  let exta1 =  xintmap_inserts (map (\x -> (x, Qbit)) [0..len-1]) xintmap_empty
  let (bcir@(c,n), a) = extract_simple (\x -> x) exta1 (circ eps)
  let (a1, gl, a2, mm) = c
--  putStrLn $ show gl
  let gl' = map quipper_gate2my_gate gl
  let gl'' = ccz2ccxs gl'
  putStrLn $ "qubits " ++ show (length eps) ++ "\n"
  putStrLn $ ".circuit_body"
  putStrLn $ "\t" ++ (intercalate "\n\t" $ map gshow gl'')

main :: IO ()
main = do
  args <- getArgs
  let file_name = head args  
  str <- readFile $ file_name
  let (eps , circ ) = parse_circuit str
  let len = length eps
  let exta1 =  xintmap_inserts (map (\x -> (x, Qbit)) [0..len-1]) xintmap_empty
  let (bcir@(c,n), a) = extract_simple (\x -> x) exta1 (circ eps)
  let (a1, gl, a2, mm) = c
  let gl' = map quipper_gate2my_gate gl
  let gl'' = ccz2ccxs gl'
  putStrLn $ "qubits " ++ show (length eps) ++ "\n"
  putStrLn $ ".circuit_body"
  putStrLn $ "\t" ++ (intercalate "\n\t" $ map gshow gl'')



parse :: String -> IO [QASM_Gate]
parse file = do
  str <- readFile $ file
  let (eps , circ ) = parse_circuit str
  let len = length eps
  let exta1 =  xintmap_inserts (map (\x -> (x, Qbit)) [0..len-1]) xintmap_empty
  let (bcir@(c,n), a) = extract_simple (\x -> x) exta1 (circ eps)
  let (a1, gl, a2, mm) = c
  preview_bcircuit bcir
  putStrLn $ show gl
  let gl' = map quipper_gate2my_gate gl
--  topdf gl'
  return $ gl'


parseQuipper :: String -> IO (Int, [QASM_Gate])
parseQuipper str = do
  let (eps , circ ) = parse_circuit str
  let len = length eps
  let exta1 =  xintmap_inserts (map (\x -> (x, Qbit)) [0..len-1]) xintmap_empty
  let (bcir@(c,n), a) = extract_simple (\x -> x) exta1 (circ eps)
  let (a1, gl, a2, mm) = c
--  preview_bcircuit bcir
--  putStrLn $ show gl
  let gl' = map quipper_gate2my_gate gl
--  topdf gl'
  return $ (mm,gl')


parseQuipper' :: String -> IO [QASM_Gate]
parseQuipper' s = do
  str <- readFile $ s
  let (eps , circ ) = parse_circuit str
  let len = length eps
  let exta1 =  xintmap_inserts (map (\x -> (x, Qbit)) [0..len-1]) xintmap_empty
  let (bcir@(c,n), a) = extract_simple (\x -> x) exta1 (circ eps)
  let (a1, gl, a2, mm) = c
--  preview_bcircuit bcir
--  putStrLn $ show gl
  let gl' = map quipper_gate2my_gate gl
--  topdf gl'
  return $ gl'


share :: Qubit -> Circ (Qubit, Qubit)
share a = do
b <- qinit False
b <- qnot b `controlled` a
return (a,b)
