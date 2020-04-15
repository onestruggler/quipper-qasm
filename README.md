# quipper-flat
Flaten Quipper ASCII format circuit into a list of gates

# Installation
1. Install Haskell-platform
2. Install Quipper
3. either a) load Quipper2QASM into ghci, then run main' "tof_3"
   or b) compile Quipper2QASM using ghc to get an executable, then run ./Quipper2QASM tof_3

# Target format
We follow the QASM format defined here:
https://www.quantum-inspire.com/kbase/qasm/