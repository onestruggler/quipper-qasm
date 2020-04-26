# quipper-qasm
Translate Quipper ASCII format circuit to OpenQASM 2.0 program.

# Installation
1. Install Haskell-platform
2. Install Quipper
3. either a) load QasmPrinting into ghci, then run main' "tof_3" , or, main'_inline "tof_3"
   or b) compile QasmPrinting using ghc to get an executable, then run ./QasmPrinting tof_3, or,  ./QasmPrinting -inline tof_3

*b) sometime ghc cannot find some packages, then using the following command to compile:

ghc -package quipper-language -package quipper-libraries -package mtl -package easyrender -package quipper-utils QasmPrinting

(if ghc still cannot find the required packages, use cabal to reinstall them)

(we also include serval test circuits downloaded from: https://github.com/njross/optimizer)

# Target language
We follow the following OpenQASM language specification:
https://arxiv.org/pdf/1707.03429.pdf
The output codes can be recoginized by IBM Quantum Experience online OpenQASM circuit editor https://quantum-computing.ibm.com/ (you need an account there which is easy and free to create)
