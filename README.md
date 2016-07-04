# Haskell-Scheme-Compiler
A project to create a R5RS compiler in Haskell. Ultimately want to target LLVM on the backend

## Build Instructions
1. Install stack
   * On mac, you can use homebrew: ```brew install haskell-stack```
  * Otherwise follow instructions here:  http://docs.haskellstack.org/en/stable/README/
  
2. Checkout this project and run ```stack build``` in top-level directory.

3. If all goes well, you're ready to go. Currently the app works takes scheme source code from STDIN.<br />
   Try this command: ```echo "(+ 2 2) | stack exec scheme-compiler-exe``` 
