module PPrint where
import CLexer
import CParser

pPrintUnary :: UnaryOper -> String
pPrintUnary UMinus    = "-"
pPrintUnary UComp     = "~"
pPrintUnary ULogicNeg = "!"

pPrintFactor :: Exp -> String
pPrintFactor (Constant i) = show i
pPrintFactor (UnaryAct (u, e)) = pPrintUnary u ++ pPrintExp e

pPrintB1 :: BinaryOper1 -> String
pPrintB1 BMult = "*"
pPrintB1 BDiv  = "/"

pPrintTerm :: Exp -> String
pPrintTerm (TBAct (o, e1, e2)) = "(" ++ pPrintExp e1 ++ pPrintB1 o ++ pPrintExp e2 ++ ")"
pPrintTerm f = pPrintFactor f

pPrintB2 :: BinaryOper2 -> String
pPrintB2 BAdd = "+"
pPrintB2 BSub = "-"

pPrintExp :: Exp -> String
pPrintExp (EBAct (o, e1, e2)) = "(" ++ pPrintExp e1 ++ pPrintB2 o ++ pPrintExp e2 ++ ")"
pPrintExp t                   = pPrintTerm t

pPrintStat :: Statement -> [String]
pPrintStat (Return exp) = ["RETURN "++pPrintExp exp]

pPrintFunc :: FunctionDecl -> [String]
pPrintFunc (FunctionDecl (n, s)) = ["FUN INT "++n++":", "  params: ()", "  body:"] ++ map ("    " ++) (pPrintStat s)

pPrintProg :: Program -> String
pPrintProg (Program f) = foldl (\acc s -> acc ++ s ++ "\n") "" $ pPrintFunc f
