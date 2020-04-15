# quipper-qasm
Translate Quipper ASCII format circuit into OpenQASM program.

# Installation
1. Install Haskell-platform
2. Install Quipper
3. either a) load Quipper2QASM into ghci, then run main' "tof_3"
   or b) compile Quipper2QASM using ghc to get an executable, then run ./Quipper2QASM tof_3

# Target language
We follow the following OpenQASM language specification:
https://arxiv.org/pdf/1707.03429.pdf
