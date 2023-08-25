module PPrint where
import CLexer
import CParser

pPrintUnary :: UnaryOper -> String
pPrintUnary UMinus    = "-"
pPrintUnary UComp     = "~"
pPrintUnary ULogicNeg = "!"

pPrintFactor :: Factor -> String
pPrintFactor (Parens e) = "(" ++ pPrintExp e ++ ")"

pPrintTerm :: Term -> String
pPrintTerm (Fac f) = pPrintFactor f

pPrintExp :: Exp -> String
pPrintExp (Ter t) = pPrintTerm t

pPrintStat :: Statement -> [String]
pPrintStat (Return exp) = ["RETURN "++pPrintExp exp]

pPrintFunc :: FunctionDecl -> [String]
pPrintFunc (FunctionDecl (n, s)) = ["FUN INT "++n++":", "  params: ()", "  body:"] ++ map ("    " ++) (pPrintStat s)

pPrintProg :: Program -> String
pPrintProg (Program f) = foldl (\acc s -> acc ++ s ++ "\n") "" $ pPrintFunc f
