module PPrint where
import CLexer
import CParser

pPrintUnary :: UnaryOper -> String
pPrintUnary UMinus    = "-"
pPrintUnary UComp     = "~"
pPrintUnary ULogicNeg = "!"

pPrintBinary :: BinaryOper -> String
pPrintBinary BMult = "*"
pPrintBinary BDiv  = "/"
pPrintBinary BAdd  = "+"
pPrintBinary BSub  = "-"
pPrintBinary BAnd  = "&&"
pPrintBinary BOr   = "||"
pPrintBinary BEqu  = "=="
pPrintBinary BNqu  = "!="
pPrintBinary BLt   = "<"
pPrintBinary BGt   = ">"
pPrintBinary BLe   = "<="
pPrintBinary BGe   = ">="

pPrintExp :: Exp -> String
pPrintExp (Var (v, _)) = "$"++v
pPrintExp (Set ((v, e), _)) = "($"++v ++ " = " ++ pPrintExp e ++ ")"
pPrintExp (Constant (i, _)) = show i
pPrintExp (UnaryAct ((o, e), _)) = pPrintUnary o ++ "(" ++ pPrintExp e ++ ")"
pPrintExp (BinAct ((o, e1, e2), _)) = "(" ++ pPrintExp e1 ++ pPrintBinary o ++ pPrintExp e2 ++ ")"

pPrintStat :: Statement -> [String]
pPrintStat (Expr e) = [pPrintExp e]
pPrintStat (Declare ((v, Nothing), _)) = ["DECLARE $" ++ v]
pPrintStat (Declare ((v, Just e),  _)) = ["DECLARE $" ++ v ++ " = " ++ pPrintExp e]
pPrintStat (Return (exp, _)) = ["RETURN "++pPrintExp exp]

pPrintFunc :: FunctionDecl -> [String]
pPrintFunc (FunctionDecl ((n, s), _)) = ["FUN INT "++n++":", "  params: ()", "  body:"] ++ concatMap (map ("    "++).pPrintStat) s

pPrintProg :: Program -> String
pPrintProg (Program (f, _)) = foldl (\acc s -> acc ++ s ++ "\n") "" $ pPrintFunc f
