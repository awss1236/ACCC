module CAssembler where
import CLexer
import CParser

genExpAsm :: Exp -> String
genExpAsm (Constant i) = "  movl   $" ++ show i ++ ", %eax\n"
genExpAsm (UnaryAct (Comp    , exp)) = genExpAsm exp ++ "  not    %eax\n"
genExpAsm (UnaryAct (Minus   , exp)) = genExpAsm exp ++ "  neg    %eax\n"
genExpAsm (UnaryAct (LogicNeg, exp)) = genExpAsm exp ++ "  cmpl   $0, %eax\n  movl   $0, %eax\n  sete   %al\n"

genStatAsm :: Statement -> String
genStatAsm (Return exp) = genExpAsm exp ++ "  ret\n"

genFuncAsm :: FunctionDecl -> String
genFuncAsm (FunctionDecl (n, s)) = " .globl _"++n++"\n_"++n++":\n"++genStatAsm s

genProgAsm :: Program -> String
genProgAsm (Program f) = genFuncAsm f
