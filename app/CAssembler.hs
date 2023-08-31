module CAssembler where
import CLexer
import CParser

genExpAsm :: Exp -> String
genExpAsm (Constant (i, _)) = "  movl   $" ++ show i ++ ", %eax\n"
genExpAsm (UnaryAct ((UComp    , exp), _)) = genExpAsm exp ++ "  not    %eax\n"
genExpAsm (UnaryAct ((UMinus   , exp), _)) = genExpAsm exp ++ "  neg    %eax\n"
genExpAsm (UnaryAct ((ULogicNeg, exp), _)) = genExpAsm exp ++ "  cmpl   $0, %eax\n  movl   $0, %eax\n  sete   %al\n"
genExpAsm (BinAct((BMult, e1, e2), _)) = genExpAsm e1 ++ "  push   %eax\n" ++ genExpAsm e2 ++ "  pop    %ecx\n  imul   %ecx, %eax\n"
genExpAsm (BinAct((BDiv,  e1, e2), _)) = genExpAsm e1 ++ "  push   %eax\n" ++ genExpAsm e2 ++ "  movl   %eax, %ecx\n  pop    %eax\n  cdq\n  idivl   %ecx\n"
genExpAsm (BinAct((BAdd,  e1, e2), _)) = genExpAsm e1 ++ "  push   %eax\n" ++ genExpAsm e2 ++ "  pop    %ecx\n  addl   %ecx, %eax\n"
genExpAsm (BinAct((BSub,  e1, e2), _)) = genExpAsm e2 ++ "  push   %eax\n" ++ genExpAsm e1 ++ "  pop    %ecx\n  subl   %ecx, %eax\n"
genExpAsm (BinAct((BAnd,  e1, e2), (x, y))) = let p = "_" ++ show x ++ "_" ++ show y ++ ":\n" in genExpAsm e1 ++ "  cmpl   $0, %eax\n  jne    _clause2" ++ p ++ "  jmp    _end" ++ p ++ "_clause2" ++ p ++ genExpAsm e2 ++ "  cmpl   $0, %eax\n  movl   $0, %eax\n  setne  %al\n_end" ++ p
genExpAsm (BinAct((BOr,   e1, e2), (x, y))) = let p = "_" ++ show x ++ "_" ++ show y ++ ":\n" in genExpAsm e1 ++ "  cmpl   $0, %eax\n  je     _clause2" ++ p ++ "  movl   $1, %eax\n  jmp    _end" ++ p ++ "_clause2" ++ p ++ genExpAsm e2 ++ "  cmpl   $0, %eax\n  movl   $0, %eax\n  setne  %al\n_end" ++ p
genExpAsm (BinAct((BEqu,  e1, e2), _)) = genExpAsm e1 ++ "  push   %eax\n" ++ genExpAsm e2 ++ "  pop    %ecx\n  cmpl   %eax, %ecx\n  movl   $0, %eax\n  sete   %al\n"
genExpAsm (BinAct((BNqu,  e1, e2), _)) = genExpAsm e1 ++ "  push   %eax\n" ++ genExpAsm e2 ++ "  pop    %ecx\n  cmpl   %eax, %ecx\n  movl   $0, %eax\n  setne  %al\n"
genExpAsm (BinAct((BLt,   e1, e2), _)) = genExpAsm e1 ++ "  push   %eax\n" ++ genExpAsm e2 ++ "  pop    %ecx\n  cmpl   %eax, %ecx\n  movl   $0, %eax\n  setl   %al\n"
genExpAsm (BinAct((BGt,   e1, e2), _)) = genExpAsm e1 ++ "  push   %eax\n" ++ genExpAsm e2 ++ "  pop    %ecx\n  cmpl   %eax, %ecx\n  movl   $0, %eax\n  setg   %al\n"
genExpAsm (BinAct((BLe,   e1, e2), _)) = genExpAsm e1 ++ "  push   %eax\n" ++ genExpAsm e2 ++ "  pop    %ecx\n  cmpl   %eax, %ecx\n  movl   $0, %eax\n  setle  %al\n"
genExpAsm (BinAct((BGe,   e1, e2), _)) = genExpAsm e1 ++ "  push   %eax\n" ++ genExpAsm e2 ++ "  pop    %ecx\n  cmpl   %eax, %ecx\n  movl   $0, %eax\n  setge  %al\n"

genStatAsm :: Statement -> String
genStatAsm (Return (exp, _)) = genExpAsm exp ++ "  ret\n"

genFuncAsm :: Bool -> FunctionDecl -> String
genFuncAsm False (FunctionDecl ((n, s), _)) = " .globl _"++n++"\n_"++n++":\n"++genStatAsm s
genFuncAsm True  (FunctionDecl ((n, s), _)) = " .globl " ++n++"\n" ++n++":\n"++genStatAsm s

genProgAsm :: Bool -> Program -> String
genProgAsm b (Program (f, _)) = genFuncAsm b f
