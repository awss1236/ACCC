module PPrint where
import CLexer
import CParser

pPrintUnary :: UnaryOper -> String
pPrintUnary Minus    = "-"
pPrintUnary Comp     = "~"
pPrintUnary LogicNeg = "!"

pPrintExp :: Exp -> String
pPrintExp (Constant i) = "INT<"++show i++">"
pPrintExp (UnaryAct (u, e)) = pPrintUnary u ++ pPrintExp e

pPrintStat :: Statement -> [String]
pPrintStat (Return exp) = ["RETURN "++pPrintExp exp]

pPrintFunc :: FunctionDecl -> [String]
pPrintFunc (FunctionDecl (n, s)) = ["FUN INT "++n++":", "  params: ()", "  body:"] ++ map ("    " ++) (pPrintStat s)

pPrintProg :: Program -> String
pPrintProg (Program f) = foldl (\acc s -> acc ++ s ++ "\n") "" $ pPrintFunc f
