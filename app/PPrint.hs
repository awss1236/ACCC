module PPrint where
import CLexer
import CParser

pPrintUnary :: UnaryOper -> String
pPrintUnary UMinus    = "-"
pPrintUnary UComp     = "~"
pPrintUnary ULogicNeg = "!"

pPrintFactor :: Exp -> String
pPrintFactor (Constant (i, _)) = show i
pPrintFactor (UnaryAct ((u, e), _)) = pPrintUnary u ++ pPrintExp e

pPrintB1 :: BinaryOper -> String
pPrintB1 BMult = "*"
pPrintB1 BDiv  = "/"

pPrintTerm :: Exp -> String
pPrintTerm (BinAct ((o, e1, e2), _)) = "(" ++ pPrintExp e1 ++ pPrintB1 o ++ pPrintExp e2 ++ ")"
pPrintTerm f = pPrintFactor f

pPrintB2 :: BinaryOper -> String
pPrintB2 BAdd = "+"
pPrintB2 BSub = "-"

pPrintExp :: Exp -> String
pPrintExp (BinAct ((o, e1, e2), _)) = "(" ++ pPrintExp e1 ++ pPrintB2 o ++ pPrintExp e2 ++ ")"
pPrintExp t                   = pPrintTerm t

pPrintStat :: Statement -> [String]
pPrintStat (Return (exp, _)) = ["RETURN "++pPrintExp exp]

pPrintFunc :: FunctionDecl -> [String]
pPrintFunc (FunctionDecl ((n, s), _)) = ["FUN INT "++n++":", "  params: ()", "  body:"] ++ concatMap (map ("    "++).pPrintStat) s

pPrintProg :: Program -> String
pPrintProg (Program (f, _)) = foldl (\acc s -> acc ++ s ++ "\n") "" $ pPrintFunc f
