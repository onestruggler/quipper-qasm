# quipper-qasm
Translate Quipper ASCII format circuit into OpenQASM 2.0 program.

# Installation
1. Install Haskell-platform
2. Install Quipper
3. either a) load QasmPrinting into ghci, then run main' "tof_3"
   or b) compile QasmPrinting using ghc to get an executable, then run ./Quipper2QASM tof_3

*b) sometime ghc cannot find some package, then using the following command to compile:

ghc -package quipper-language -package quipper-libraries -package mtl -package easyrender -package quipper-utils QasmPrinting

(if ghc still cannot find the required package, try cabal to reinstall them)
(we also include serval test circuit downloaded from: https://github.com/njross/optimizer)

# Target language
We follow the following OpenQASM language specification:
https://arxiv.org/pdf/1707.03429.pdf
The output codes can be recoginized by IBM Quantum Experience online OpenQASM circuit editor https://quantum-computing.ibm.com/ (you need an account there which is free to open)
