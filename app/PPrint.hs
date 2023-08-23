module PPrint where
import CParser

pPrintExp :: Exp -> String
pPrintExp (Constant i) = "INT<"++show i++">"
pPrintExp (UnaryOper (c, e)) = c : pPrintExp e

pPrintStat :: Statement -> [String]
pPrintStat (Return exp) = ["RETURN "++pPrintExp exp]

pPrintFunc :: FunctionDecl -> [String]
pPrintFunc (FunctionDecl (n, s)) = ["FUN INT "++n++":", "  params: ()", "  body:"] ++ map ("    " ++) (pPrintStat s)

pPrintProg :: Program -> String
pPrintProg (Program f) = foldl (\acc s -> acc ++ s ++ "\n") "" $ pPrintFunc f
