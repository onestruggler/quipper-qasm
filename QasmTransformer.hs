module QasmTransformer where

import Quipper
import Quipper.Libraries.QuipperASCIIParser
import Quipper.Libraries.GateDecompositions
import Quipper.Libraries.Decompose
import Quipper.Libraries.Decompose.CliffordT
import Quipper.Libraries.Decompose.Legacy
import Quipper.Internal.Monad 

import System.Environment
import System.Exit
import System.IO


main1' :: String -> IO ()
main1' f = do
  str <- readFile f

  let (ins,circuit) = parse_circuit str
  let decomposed_circuit = qasm_generic circuit
  print_generic ASCII decomposed_circuit ins
  print_generic Preview decomposed_circuit ins  


with_combined_controls_toffoli :: Int -> [Signed Endpoint] -> ([Signed Qubit] -> Circ a) -> Circ a
with_combined_controls_toffoli = with_combined_controls toffoli_plain_at


-- ----------------------------------------------------------------------
-- ** Control trimming

-- | This transformer makes sure that not-gates, Pauli /X/-, /Y/-, and
-- /Z/-gates, and /X/-gates have at most two controls; that phase
-- gates of the form Diag(1, φ) have no controls; and that all other
-- gates have at most one control. 
trimctrls_tof_transformer :: Transformer Circ Qubit Bit
trimctrls_tof_transformer (T_QGate "not" 1 0 _ ncf f) = f $
  \[q] [] cs -> without_controls_if ncf $ do
    with_combined_controls_toffoli 2 cs $ \qcs -> do
      qnot_at q `controlled` qcs
      return ([q], [], cs)
trimctrls_tof_transformer (T_QGate "multinot" _ 0 _ ncf f) = f $
  \qs [] cs -> without_controls_if ncf $ do
    with_combined_controls_toffoli 1 cs $ \qcs -> do
      qmultinot qs `controlled` qcs
      return (qs, [], cs)
trimctrls_tof_transformer (T_QGate "H" 1 0 _ ncf f) = f $
  \[q] [] cs -> without_controls_if ncf $ do
    with_combined_controls_toffoli 1 cs $ \qcs -> do
      hadamard q `controlled` qcs
      return ([q], [], cs)
trimctrls_tof_transformer (T_QGate "swap" 2 0 _ ncf f) = f $
  \[q1, q2] [] cs -> without_controls_if ncf $ do
    with_combined_controls_toffoli 1 cs $ \qcs -> do
      case qcs of
        -- one control
        [c] -> do
          qnot_at q1 `controlled` q2
          qnot_at q2 `controlled` [c,(Signed q1 True)] 
          qnot_at q1 `controlled` q2
          return ([q1, q2], [], cs)
        -- zero controls
        qcs -> do 
          swap_at q1 q2
          return ([q1, q2], [], cs)
trimctrls_tof_transformer (T_QGate "W" 2 0 _ ncf f) = f $
  \[q1, q2] [] cs -> without_controls_if ncf $ do
    with_combined_controls_toffoli 1 cs $ \qcs -> do
      case qcs of
        -- one control
        [c] -> do 
          qnot q2 `controlled` q1
          w' <- qinit_qubit False
          qnot_at w' `controlled`  [c,(Signed q2 True)]
          hadamard q1 `controlled` w'
          qnot_at w' `controlled` [c,(Signed q2 True)]
          qterm_qubit False w' 
          qnot q2 `controlled` q1
          return ([q1, q2], [], cs)
        -- zero controls
        qcs -> do 
          gate_W q1 q2
          return ([q1, q2], [], cs)
trimctrls_tof_transformer gate@(T_QGate name _ _ inv ncf f) = f $
  \qs gctls cs -> without_controls_if ncf $ do
    let n = case (name, qs, gctls) of
          -- certain gates are allowed two controls
          ("X", [q], []) -> 2
          ("not", [q], []) -> 2          
--          ("Y", [q], []) -> 2
--          ("Z", [q], []) -> 2
--          ("iX", [q], []) -> 2
          _ -> 1
    with_combined_controls_toffoli n cs $ \qcs -> do
      named_gate_qulist_at name inv qs gctls `controlled` qcs
      return (qs,gctls,cs)
trimctrls_tof_transformer (T_GPhase t ncf f) = f $
  \qs cs -> without_controls_if ncf $ do
    with_combined_controls_toffoli 1 cs $ \qcs -> do
      global_phase_anchored t qs `controlled` qcs
      return cs
trimctrls_tof_transformer gate@(T_QRot name _ _ inv theta ncf f) = f $
  \qs gctls cs -> without_controls_if ncf $ do
    let n = case (name, qs, gctls) of
          -- certain gates are decomposed to zero controls
          ("R(2pi/%)", [q], []) -> 0
          ("T(%)", [q], []) -> 0
          _ -> 1
    case n of
      0 -> do
        let [q] = qs
        with_combined_controls_toffoli 1 (Signed (Endpoint_Qubit q) True : cs) $ \qcs -> do
          let [c] = qcs
          with_signed_qubit c $ \q -> do
            named_rotation_qulist_at name inv theta [q] gctls
            return (qs,gctls,cs)
      _ -> do
        with_combined_controls_toffoli 1 cs $ \qcs -> do
          named_rotation_qulist_at name inv theta qs gctls `controlled` qcs
          return (qs,gctls,cs)

-- The subroutine transformer clause is called when a subroutine gate appears,
-- for now we decompose the controls just like for other gates. The recursive
-- decomposition of a subroutine is taken care of in the dynamic transformer.
trimctrls_tof_transformer (T_Subroutine n inv ncf scf ws_pat a1 vs_pat a2 rep f) = f $
  \namespace ws cs -> without_controls_if ncf $ do
    let qws = [w | Endpoint_Qubit w <- ws]
    provide_subroutines namespace
    with_combined_controls_toffoli 1 cs $ \qcs -> do
      vs <- subroutine n inv scf rep ws_pat a1 vs_pat a2 ws `controlled` qcs
      return (vs,cs)

-- We list catch-all cases explicitly, so that the type-checker can
-- warn about new gates that must be added to the list.
trimctrls_tof_transformer gate@(T_CNot _ _) = identity_transformer gate
trimctrls_tof_transformer gate@(T_CGate _ _ _) = identity_transformer gate
trimctrls_tof_transformer gate@(T_CGateInv _ _ _) = identity_transformer gate
trimctrls_tof_transformer gate@(T_CSwap _ _) = identity_transformer gate
trimctrls_tof_transformer gate@(T_QPrep _ _) = identity_transformer gate
trimctrls_tof_transformer gate@(T_QUnprep _ _) = identity_transformer gate
trimctrls_tof_transformer gate@(T_QInit _ _ _) = identity_transformer gate
trimctrls_tof_transformer gate@(T_CInit _ _ _) = identity_transformer gate
trimctrls_tof_transformer gate@(T_QTerm _ _ _) = identity_transformer gate
trimctrls_tof_transformer gate@(T_CTerm _ _ _) = identity_transformer gate
trimctrls_tof_transformer gate@(T_QMeas _) = identity_transformer gate
trimctrls_tof_transformer gate@(T_QDiscard _) = identity_transformer gate
trimctrls_tof_transformer gate@(T_CDiscard _) = identity_transformer gate
trimctrls_tof_transformer gate@(T_DTerm _ _) = identity_transformer gate
trimctrls_tof_transformer gate@(T_Comment _ _ _) = identity_transformer gate
{-
qasm_tof_transformer :: Transformer Circ Qubit Bit
qasm_tof_transformer (T_QGate n _ _ _ ncf f) = f $
  \ts gcs ctrls -> do
    without_controls_if ncf $ do
      with_combined_controls_gb GB_Toffoli 2 ctrls $ \qcs -> do
        
        return (ts, gcs, ctrls)
qasm_tof_transformer g = identity_transformer g
-}

normalctrls_transformer :: Transformer Circ Qubit Bit
normalctrls_transformer gate@(T_QGate name _ _ inv ncf f) = f $
  \qs gctls cs -> without_controls_if ncf $ do
    with_normalized_controls cs $ \qcs -> do
      named_gate_qulist_at name inv qs gctls `controlled` qcs
      return (qs,gctls,cs)
normalctrls_transformer gate@(T_QRot name _ _ inv theta ncf f) = f $
  \qs gctls cs -> without_controls_if ncf $ do
    with_normalized_controls cs $ \qcs -> do
      named_rotation_qulist_at name inv theta qs gctls `controlled` qcs
      return (qs,gctls,cs)
normalctrls_transformer (T_GPhase t ncf f) = f $
  \qs cs -> without_controls_if ncf $ do
    with_normalized_controls cs $ \qcs -> do
      global_phase_anchored t qs `controlled` qcs
      return cs
-- The subroutine transformer clause is called when a subroutine gate appears,
-- for now we decompose the controls just like for other gates. The recursive
-- decomposition of a subroutine is taken care of in the dynamic transformer.
normalctrls_transformer (T_Subroutine n inv ncf scf ws_pat a1 vs_pat a2 rep f) = f $
  \namespace ws cs -> without_controls_if ncf $ do
    let qws = [w | Endpoint_Qubit w <- ws]
    provide_subroutines namespace
    with_normalized_controls cs $ \qcs -> do
      vs <- subroutine n inv scf rep ws_pat a1 vs_pat a2 ws `controlled` qcs
      return (vs,cs)
-- We list catch-all cases explicitly, so that the type-checker can
-- warn about new gates that must be added to the list.
normalctrls_transformer gate@(T_CNot _ _) = identity_transformer gate
normalctrls_transformer gate@(T_CGate _ _ _) = identity_transformer gate
normalctrls_transformer gate@(T_CGateInv _ _ _) = identity_transformer gate
normalctrls_transformer gate@(T_CSwap _ _) = identity_transformer gate
normalctrls_transformer gate@(T_QPrep _ _) = identity_transformer gate
normalctrls_transformer gate@(T_QUnprep _ _) = identity_transformer gate
normalctrls_transformer gate@(T_QInit _ _ _) = identity_transformer gate
normalctrls_transformer gate@(T_CInit _ _ _) = identity_transformer gate
normalctrls_transformer gate@(T_QTerm _ _ _) = identity_transformer gate
normalctrls_transformer gate@(T_CTerm _ _ _) = identity_transformer gate
normalctrls_transformer gate@(T_QMeas _) = identity_transformer gate
normalctrls_transformer gate@(T_QDiscard _) = identity_transformer gate
normalctrls_transformer gate@(T_CDiscard _) = identity_transformer gate
normalctrls_transformer gate@(T_DTerm _ _) = identity_transformer gate
normalctrls_transformer gate@(T_Comment _ _ _) = identity_transformer gate



qasm_generic cir = cir' where
  cir' = transform_generic normalctrls_transformer c2
  c2 = transform_generic trimctrls_tof_transformer cir
