module CAssembler where
import CLexer
import CParser
import qualified Data.Map as M

type StackState = (Int, M.Map String Int)

genExpAsm :: StackState -> Exp -> String
genExpAsm _ (Constant (i, _)) = "  movl   $" ++ show i ++ ", %eax\n"
genExpAsm m (UnaryAct ((UComp    , exp), _)) = genExpAsm m exp ++ "  not    %eax\n"
genExpAsm m (UnaryAct ((UMinus   , exp), _)) = genExpAsm m exp ++ "  neg    %eax\n"
genExpAsm m (UnaryAct ((ULogicNeg, exp), _)) = genExpAsm m exp ++ "  cmpl   $0, %eax\n  movl   $0, %eax\n  sete   %al\n"
genExpAsm m (BinAct((BMult, e1, e2), _)) = genExpAsm m e1 ++ "  push   %eax\n" ++ genExpAsm m e2 ++ "  pop    %ecx\n  imul   %ecx, %eax\n"
genExpAsm m (BinAct((BDiv,  e1, e2), _)) = genExpAsm m e1 ++ "  push   %eax\n" ++ genExpAsm m e2 ++ "  movl   %eax, %ecx\n  pop    %eax\n  cdq\n  idivl   %ecx\n"
genExpAsm m (BinAct((BAdd,  e1, e2), _)) = genExpAsm m e1 ++ "  push   %eax\n" ++ genExpAsm m e2 ++ "  pop    %ecx\n  addl   %ecx, %eax\n"
genExpAsm m (BinAct((BSub,  e1, e2), _)) = genExpAsm m e2 ++ "  push   %eax\n" ++ genExpAsm m e1 ++ "  pop    %ecx\n  subl   %ecx, %eax\n"
genExpAsm m (BinAct((BAnd,  e1, e2), (x, y))) = let p = "_" ++ show x ++ "_" ++ show y in genExpAsm m e1 ++ "  cmpl   $0, %eax\n  jne    _clause2" ++ p ++ "\n  jmp    _end" ++ p ++ "\n_clause2" ++ p ++ ":\n" ++ genExpAsm m e2 ++ "  cmpl   $0, %eax\n  movl   $0, %eax\n  setne  %al\n_end" ++ p ++ ":\n"
genExpAsm m (BinAct((BOr,   e1, e2), (x, y))) = let p = "_" ++ show x ++ "_" ++ show y ++ ":\n" in genExpAsm m e1 ++ "  cmpl   $0, %eax\n  je     _clause2" ++ p ++ "\n  movl   $1, %eax\n  jmp    _end" ++ p ++ "\n_clause2" ++ p ++ ":\n" ++ genExpAsm m e2 ++ "  cmpl   $0, %eax\n  movl   $0, %eax\n  setne  %al\n_end" ++ p ++ ":\n"
genExpAsm m (BinAct((BEqu,  e1, e2), _)) = genExpAsm m e1 ++ "  push   %eax\n" ++ genExpAsm m e2 ++ "  pop    %ecx\n  cmpl   %eax, %ecx\n  movl   $0, %eax\n  sete   %al\n"
genExpAsm m (BinAct((BNqu,  e1, e2), _)) = genExpAsm m e1 ++ "  push   %eax\n" ++ genExpAsm m e2 ++ "  pop    %ecx\n  cmpl   %eax, %ecx\n  movl   $0, %eax\n  setne  %al\n"
genExpAsm m (BinAct((BLt,   e1, e2), _)) = genExpAsm m e1 ++ "  push   %eax\n" ++ genExpAsm m e2 ++ "  pop    %ecx\n  cmpl   %eax, %ecx\n  movl   $0, %eax\n  setl   %al\n"
genExpAsm m (BinAct((BGt,   e1, e2), _)) = genExpAsm m e1 ++ "  push   %eax\n" ++ genExpAsm m e2 ++ "  pop    %ecx\n  cmpl   %eax, %ecx\n  movl   $0, %eax\n  setg   %al\n"
genExpAsm m (BinAct((BLe,   e1, e2), _)) = genExpAsm m e1 ++ "  push   %eax\n" ++ genExpAsm m e2 ++ "  pop    %ecx\n  cmpl   %eax, %ecx\n  movl   $0, %eax\n  setle  %al\n"
genExpAsm m (BinAct((BGe,   e1, e2), _)) = genExpAsm m e1 ++ "  push   %eax\n" ++ genExpAsm m e2 ++ "  pop    %ecx\n  cmpl   %eax, %ecx\n  movl   $0, %eax\n  setge  %al\n"
genExpAsm m (Set((n, e), _)) = genExpAsm m e ++ "  movl   %eax, " ++ show (snd m M.! n) ++ "(%ebp)\n"
genExpAsm m (Var(n, _)) = "  movl   " ++ show (snd m M.! n) ++ "(%ebp), %eax\n"


genStatAsm :: StackState -> Statement -> (String, StackState)
genStatAsm im (Return (exp, _)) = (genExpAsm im exp ++ "  movl   %ebp, %esp\n  pop    %ebp\n  ret\n", im)
genStatAsm im (Expr exp)        = (genExpAsm im exp, im)
genStatAsm (i, m) (Declare ((n, Nothing), _)) = (if M.member n m then "Variable redeclaration is a fucked up thing bud" else "  pushl  $69\n", (i-4, M.insert n i m))
genStatAsm (i, m) (Declare ((n, Just e ), _)) = (if M.member n m then "Variable redeclaration is a fucked up thing bud" else genExpAsm (i, m) e ++ "  push   %eax\n", (i-4, M.insert n i m))

genFuncAsm :: Bool -> FunctionDecl -> String
genFuncAsm False (FunctionDecl ((n, s), _)) = " .globl _"++n++"\n_"++n++":\n  push   %ebp\n  movl   %esp, %ebp\n"++ fst (foldl (\(a, stk) stt -> let (na, nstk) = genStatAsm stk stt in (a ++ na, nstk)) ("", (-4, M.empty)) s) ++ mReturnZero (last s)
      where mReturnZero (Return _) = ""
            mReturnZero _ = "  movl   %ebp, %esp\n  pop    %ebp\n  movl   $0, %eax\n  ret\n"
genFuncAsm True  (FunctionDecl ((n, s), _)) = " .globl " ++n++"\n" ++n++":\n  push   %ebp\n  movl   %esp, %ebp\n"++ fst (foldl (\(a, stk) stt -> let (na, nstk) = genStatAsm stk stt in (a ++ na, nstk)) ("", (-4, M.empty)) s) ++ mReturnZero (last s)
      where mReturnZero (Return _) = ""
            mReturnZero _ = "  movl   %ebp, %esp\n  pop    %ebp\n  movl   $0, %eax\n  ret\n"

genProgAsm :: Bool -> Program -> String
genProgAsm b (Program (f, _)) = genFuncAsm b f
