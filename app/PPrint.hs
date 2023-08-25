module PPrint where
import CLexer
import CParser

pPrintUnary :: UnaryOper -> String
pPrintUnary UMinus    = "-"
pPrintUnary UComp     = "~"
pPrintUnary ULogicNeg = "!"

pPrintFactor :: Factor -> String
pPrintFactor (Constant i) = show i
pPrintFactor (Parens e) = "(" ++ pPrintExp e ++ ")"
pPrintFactor (UnaryAct (u, f)) = pPrintUnary u ++ pPrintFactor f

pPrintB1 :: BinaryOper1 -> String
pPrintB1 BMult = "*"
pPrintB1 BDiv  = "/"

pPrintTerm :: Term -> String
pPrintTerm (Fac f) = pPrintFactor f
pPrintTerm (TBAct (o, t, f)) = pPrintTerm t ++ pPrintB1 o ++ pPrintFactor f

pPrintB2 :: BinaryOper2 -> String
pPrintB2 BAdd = "+"
pPrintB2 BSub = "-"

pPrintExp :: Exp -> String
pPrintExp (Ter t) = pPrintTerm t
pPrintExp (EBAct (o, e, t)) = pPrintExp e ++ pPrintB2 o ++ pPrintTerm t

pPrintStat :: Statement -> [String]
pPrintStat (Return exp) = ["RETURN "++pPrintExp exp]

pPrintFunc :: FunctionDecl -> [String]
pPrintFunc (FunctionDecl (n, s)) = ["FUN INT "++n++":", "  params: ()", "  body:"] ++ map ("    " ++) (pPrintStat s)

pPrintProg :: Program -> String
pPrintProg (Program f) = foldl (\acc s -> acc ++ s ++ "\n") "" $ pPrintFunc f
