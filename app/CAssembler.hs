module CAssembler where
import CLexer
import CParser

genExpAsm :: Exp -> String
genExpAsm (Constant i) = "  movl   $" ++ show i ++ ", %eax\n"
genExpAsm (UnaryAct (UComp    , exp)) = genExpAsm exp ++ "  not    %eax\n"
genExpAsm (UnaryAct (UMinus   , exp)) = genExpAsm exp ++ "  neg    %eax\n"
genExpAsm (UnaryAct (ULogicNeg, exp)) = genExpAsm exp ++ "  cmpl   $0, %eax\n  movl   $0, %eax\n  sete   %al\n"
genExpAsm (BinAct(BMult, e1, e2)) = genExpAsm e1 ++ "  push   %eax\n" ++ genExpAsm e2 ++ "  pop    %ecx\n  imul   %ecx, %eax\n"
genExpAsm (BinAct(BDiv,  e1, e2)) = genExpAsm e1 ++ "  push   %eax\n" ++ genExpAsm e2 ++ "  movl   %eax, %ecx\n  pop    %eax\n  cdq\n  idivl   %ecx\n"
genExpAsm (BinAct(BAdd,  e1, e2)) = genExpAsm e1 ++ "  push   %eax\n" ++ genExpAsm e2 ++ "  pop    %ecx\n  addl   %ecx, %eax\n"
genExpAsm (BinAct(BSub,  e1, e2)) = genExpAsm e2 ++ "  push   %eax\n" ++ genExpAsm e1 ++ "  pop    %ecx\n  subl   %ecx, %eax\n"
genExpAsm (BinAct(BAnd,  e1, e2)) = genExpAsm e1 ++ "  push   %eax\n" ++ genExpAsm e2 ++ "  pop    %ecx\n"
genExpAsm (BinAct(BOr,   e1, e2)) = genExpAsm e1 ++ "  push   %eax\n" ++ genExpAsm e2 ++ "  pop    %ecx\n"
genExpAsm (BinAct(BEqu,  e1, e2)) = genExpAsm e1 ++ "  push   %eax\n" ++ genExpAsm e2 ++ "  pop    %ecx\n  cmpl   %eax, %ecx\n  movl   $0, %eax\n  sete   %al\n"
genExpAsm (BinAct(BNqu,  e1, e2)) = genExpAsm e1 ++ "  push   %eax\n" ++ genExpAsm e2 ++ "  pop    %ecx\n  cmpl   %eax, %ecx\n  movl   $0, %eax\n  setne  %al\n"
genExpAsm (BinAct(BLt,   e1, e2)) = genExpAsm e1 ++ "  push   %eax\n" ++ genExpAsm e2 ++ "  pop    %ecx\n  cmpl   %eax, %ecx\n  movl   $0, %eax\n  setl   %al\n"
genExpAsm (BinAct(BGt,   e1, e2)) = genExpAsm e1 ++ "  push   %eax\n" ++ genExpAsm e2 ++ "  pop    %ecx\n  cmpl   %eax, %ecx\n  movl   $0, %eax\n  setg   %al\n"
genExpAsm (BinAct(BLe,   e1, e2)) = genExpAsm e1 ++ "  push   %eax\n" ++ genExpAsm e2 ++ "  pop    %ecx\n  cmpl   %eax, %ecx\n  movl   $0, %eax\n  setle  %al\n"
genExpAsm (BinAct(BGe,   e1, e2)) = genExpAsm e1 ++ "  push   %eax\n" ++ genExpAsm e2 ++ "  pop    %ecx\n  cmpl   %eax, %ecx\n  movl   $0, %eax\n  setge  %al\n"

genStatAsm :: Statement -> String
genStatAsm (Return exp) = genExpAsm exp ++ "  ret\n"

genFuncAsm :: FunctionDecl -> String
genFuncAsm (FunctionDecl (n, s)) = " .globl _"++n++"\n_"++n++":\n"++genStatAsm s

genProgAsm :: Program -> String
genProgAsm (Program f) = genFuncAsm f
