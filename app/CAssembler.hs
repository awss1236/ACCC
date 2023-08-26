module CAssembler where
import CLexer
import CParser

genExpAsm :: Exp -> String
genExpAsm = undefined

genStatAsm :: Statement -> String
genStatAsm (Return exp) = genExpAsm exp ++ "  ret\n"

genFuncAsm :: FunctionDecl -> String
genFuncAsm (FunctionDecl (n, s)) = " .globl _"++n++"\n_"++n++":\n"++genStatAsm s

genProgAsm :: Program -> String
genProgAsm (Program f) = genFuncAsm f
