module CAssembler where
import CParser
import qualified Data.Map as M

type VarMap = M.Map String String
type StackState = (Int, VarMap)
type Context = ((StackState, [String]), String)

genExpAsm :: StackState -> Exp -> String
genExpAsm _ (Constant (i, _)) = "  movl   $"++show i++", %eax\n"
genExpAsm m (UnaryAct ((UComp    , exp), _)) = genExpAsm m exp++"  not    %eax\n"
genExpAsm m (UnaryAct ((UMinus   , exp), _)) = genExpAsm m exp++"  neg    %eax\n"
genExpAsm m (UnaryAct ((ULogicNeg, exp), _)) = genExpAsm m exp++"  cmpl   $0, %eax\n  movl   $0, %eax\n  sete   %al\n"
genExpAsm m (BinAct((BMult, e1, e2), _)) = genExpAsm m e1++"  push   %eax\n"++genExpAsm m e2++"  pop    %ecx\n  imul   %ecx, %eax\n"
genExpAsm m (BinAct((BDiv,  e1, e2), _)) = genExpAsm m e1++"  push   %eax\n"++genExpAsm m e2++"  movl   %eax, %ecx\n  pop    %eax\n  cdq\n  idivl   %ecx\n"
genExpAsm m (BinAct((BAdd,  e1, e2), _)) = genExpAsm m e1++"  push   %eax\n"++genExpAsm m e2++"  pop    %ecx\n  addl   %ecx, %eax\n"
genExpAsm m (BinAct((BSub,  e1, e2), _)) = genExpAsm m e2++"  push   %eax\n"++genExpAsm m e1++"  pop    %ecx\n  subl   %ecx, %eax\n"
genExpAsm m (BinAct((BAnd,  e1, e2), (x, y))) = let p = "_"++show x++"_"++show y in genExpAsm m e1++"  cmpl   $0, %eax\n  jne    _clause2"++p++"\n  jmp    _end"++p++"\n_clause2"++p++":\n"++genExpAsm m e2++"  cmpl   $0, %eax\n  movl   $0, %eax\n  setne  %al\n_end"++p++":\n"
genExpAsm m (BinAct((BOr,   e1, e2), (x, y))) = let p = "_"++show x++"_"++show y in genExpAsm m e1++"  cmpl   $0, %eax\n  je     _clause2"++p++"\n  movl   $1, %eax\n  jmp    _end"++p++"\n_clause2"++p++":\n"++genExpAsm m e2++"  cmpl   $0, %eax\n  movl   $0, %eax\n  setne  %al\n_end"++p++":\n"
genExpAsm m (BinAct((BEqu,  e1, e2), _)) = genExpAsm m e1++"  push   %eax\n"++genExpAsm m e2++"  pop    %ecx\n  cmpl   %eax, %ecx\n  movl   $0, %eax\n  sete   %al\n"
genExpAsm m (BinAct((BNqu,  e1, e2), _)) = genExpAsm m e1++"  push   %eax\n"++genExpAsm m e2++"  pop    %ecx\n  cmpl   %eax, %ecx\n  movl   $0, %eax\n  setne  %al\n"
genExpAsm m (BinAct((BLt,   e1, e2), _)) = genExpAsm m e1++"  push   %eax\n"++genExpAsm m e2++"  pop    %ecx\n  cmpl   %eax, %ecx\n  movl   $0, %eax\n  setl   %al\n"
genExpAsm m (BinAct((BGt,   e1, e2), _)) = genExpAsm m e1++"  push   %eax\n"++genExpAsm m e2++"  pop    %ecx\n  cmpl   %eax, %ecx\n  movl   $0, %eax\n  setg   %al\n"
genExpAsm m (BinAct((BLe,   e1, e2), _)) = genExpAsm m e1++"  push   %eax\n"++genExpAsm m e2++"  pop    %ecx\n  cmpl   %eax, %ecx\n  movl   $0, %eax\n  setle  %al\n"
genExpAsm m (BinAct((BGe,   e1, e2), _)) = genExpAsm m e1++"  push   %eax\n"++genExpAsm m e2++"  pop    %ecx\n  cmpl   %eax, %ecx\n  movl   $0, %eax\n  setge  %al\n"
genExpAsm m (Set((n, e), _)) = genExpAsm m e++"  movl   %eax, "++snd m M.! n++"\n"
genExpAsm m (Var(n, _)) = "  movl   "++snd m M.! n++", %eax\n"
genExpAsm m (Tern ((c, e1, e2), (x, y))) = let p = "_"++show x++"_"++show y in genExpAsm m c++"  cmpl   $0, %eax\n  je     _els"++p++"\n"++genExpAsm m e1++"  jmp    _end"++p++"\n_els"++p++":\n"++genExpAsm m e2++"_end"++p++":\n"
genExpAsm m (Call ((f, args), _)) = concat (reverse (map (\e -> genExpAsm m e++"  push   %eax\n") args))++"  call   "++f++"\n  addl   $"++show (4*length args)++", %esp\n"

genStatementAsm :: (StackState, String) -> Statement -> (String, String)
genStatementAsm (im, l) (Return (exp, _)) = (genExpAsm im exp++"  movl   %ebp, %esp\n  pop    %ebp\n  ret\n", l)
genStatementAsm (im, l) (Expr (Just exp)) = (genExpAsm im exp, l)
genStatementAsm (im, l) (Expr Nothing)    = ("# You got an empty expression in there fr fr.\n", l)
genStatementAsm (im, l) (Scope (bs, _))   = (genBlockAsm (im, l) bs, l)
genStatementAsm (_ , l) (Break) = ("  jmp    _end"++l++"\n", l)
genStatementAsm (_ , l) (Continue) = ("  jmp    _cont"++l++"\n", l)
genStatementAsm (im, l) (If ((e, s, Nothing), (x, y))) = (let p = "_"++show x++"_"++show y in genExpAsm im e++"  cmpl   $0, %eax\n  je     _end"++p++"\n"++fst (genStatementAsm (im, l) s)++"_end"++p++":\n", l)
genStatementAsm (im, l) (If ((e, s, Just s'), (x, y))) = (let p = "_"++show x++"_"++show y in genExpAsm im e++"  cmpl   $0, %eax\n  je     _els"++p++"\n"++fst (genStatementAsm (im, l) s)++"  jmp    _end"++p++"\n_els"++p++":\n"++fst (genStatementAsm (im, l) s')++"_end"++p++":\n", l)
genStatementAsm (im, l) (While ((e, s), (x, y))) = (let p = "_"++show x++"_"++show y in "_while"++p++":\n_cont"++p++":\n"++genExpAsm im e++"  cmpl   $0, %eax\n  je     _end"++p++"\n"++fst (genStatementAsm (im, p) s)++"  jmp    _while"++p++"\n_end"++p++":\n", l)
genStatementAsm (im, l) (Do ((s, e), (x, y))) = (let p = "_"++show x++"_"++show y in "_do"++p++":\n"++fst (genStatementAsm (im, p) s)++"_cont"++p++":\n"++genExpAsm im e++"  cmpl   $0, %eax\n  jne    _do"++p++"\n", l)
genStatementAsm (im, l) (For ((me1, e, me2, s), (x, y))) = (let p = "_"++show x++"_"++show y in genMExpAsm im me1++"_for"++p++":\n"++genExpAsm im e++"  cmpl   $0, %eax\n  je     _end"++p++"\n"++fst (genStatementAsm (im, p) s)++"_cont"++p++":\n"++genMExpAsm im me2++"  jmp    _for"++p++"\n_end"++p++":\n", l)
             where genMExpAsm :: StackState -> Maybe Exp -> String
                   genMExpAsm _ Nothing = ""
                   genMExpAsm im (Just e) = genExpAsm im e
genStatementAsm (im, l) (ForDecl ((dec, e, me, s), p)) = (genBlockAsm (im, "UNREACHABLE") [Decl dec, Stat $ For ((Nothing, e, me, s), p)], l)

genDeclarationAsm :: (StackState, [String]) -> Declare -> (String, (StackState, [String]))
genDeclarationAsm ((i, m), s) (Declare ((n, Nothing), _)) = if n `elem` s then ("Variable redeclared within same scope bud.", ((i, m), s)) else ("  pushl  $69\n", ((i-4, M.insert n (show i++"(%ebp)") m), n:s))
genDeclarationAsm ((i, m), s) (Declare ((n, Just e ), _)) = if n `elem` s then ("Variable redeclared within same scope bud.", ((i, m), s)) else (genExpAsm (i, m) e++"  pushl  %eax\n", ((i-4, M.insert n (show i++"(%ebp)") m), n:s))

genBlockItemAsm :: Context -> BlockItem -> (String, Context)
genBlockItemAsm ((stk, scp), l) (Stat stt) = let (asm, l') = genStatementAsm (stk, l) stt in (asm, ((stk, scp), l'))
genBlockItemAsm (sc, l) (Decl dec) = let (asm, sc') = genDeclarationAsm sc dec in (asm, (sc', l))

genBlockAsm :: (StackState, String) -> [BlockItem] -> String
genBlockAsm (stk, l) bs = let (asm, ((_, scp), _)) = foldl (\(asm, s) b -> let (asm', s') = genBlockItemAsm s b in (asm++asm', s')) ("", ((stk, []), l)) bs
                     in asm++"  addl   $"++show (length scp * 4)++", %esp\n"

genFuncAsm :: VarMap -> FunctionDecl -> String
genFuncAsm _ (FunctionDecl ((_, _, Nothing), _))   = "\n"
genFuncAsm _ (FunctionDecl ((n, _, Just []), _))   = " .globl "++n++"\n"++n++":\n  movl   $0, %eax\n  ret\n"
genFuncAsm v (FunctionDecl ((n, args, Just b), _)) = " .globl "++n++"\n"++n++":\n  push   %ebp\n  movl   %esp, %ebp\n"++genBlockAsm ((-4, v `M.union` (M.fromList $ toStack args)), "NOT_IN_LOOP") b++mReturnZero (last b)
      where mReturnZero (Stat (Return _)) = ""
            mReturnZero _ = "  movl   %ebp, %esp\n  pop    %ebp\n  movl   $42, %eax\n  ret\n"
            toStack :: [String] -> [(String, String)]
            toStack ss = map (\(s, i) -> (s, show i)) $ toStack' ss
            toStack' :: [String] -> [(String, Int)]
            toStack' [] = []
            toStack' (s:ss) = (s, 8) : (map (\(a, i) -> (a, i+4)) $ toStack' ss)

genToplevelAsm :: VarMap -> ToplevelItem -> (String, VarMap)
genToplevelAsm v (Func f) = (genFuncAsm v f, v)
genToplevelAsm v (GVar (Declare ((n, Nothing), _))) = (" .globl _"++n++"\n .bss\n .align 4\n_"++n++":\n .zero 4\n .text\n", M.insert n ("_"++n) v)
genToplevelAsm v (GVar (Declare ((n, Just (Constant (0, _))), _))) = (" .globl _"++n++"\n .bss\n .align 4\n_"++n++":\n .zero 4\n .text\n", M.insert n ("_"++n) v)
genToplevelAsm v (GVar (Declare ((n, Just (Constant (i, _))), _))) = (" .globl _"++n++"\n .data\n .align 4\n_"++n++":\n .long "++show i++"\n .text\n", M.insert n ("_"++n) v)

genProgAsm :: Program -> String
genProgAsm (Program ts) = fst $ foldl (\(asm, s) t -> let (asm', s') = genToplevelAsm s t in (asm++asm', s')) ("", M.empty) ts
