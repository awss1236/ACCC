# ACCC
A usual (unfinished) c compiler.
# Inspiration
I just wanted to make something big in haskell for once.
And I came across writing a c compiler series by Nora Sandler:
https://norasandler.com/2017/11/29/Write-a-Compiler.html
## Building
A normal ```cabal build``` would lead to having the executable a few hundred miles away so I made the build.sh (build.bat for windows too).
## Usage
Simply input the source file name to the executable and you'll get a compiled output at the same directory of the source file.
```
sccmp /.../main.c
```
