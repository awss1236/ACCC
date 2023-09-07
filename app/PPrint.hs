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
pPrintExp (Tern ((c, e1, e2), _)) = "(IF " ++ pPrintExp c ++ " THEN " ++ pPrintExp e1 ++ " ELSE " ++ pPrintExp e2 ++ ")"

pPrintBlockItem :: BlockItem -> [String]
pPrintBlockItem (Stat (Expr e)) = [pPrintExp e]
pPrintBlockItem (Stat (Return (exp, _))) = ["RETURN "++pPrintExp exp]
pPrintBlockItem (Stat (If ((e, s, Nothing), _))) = ("IF " ++ pPrintExp e ++ ":") : map ("  " ++) (pPrintBlockItem (Stat s))
pPrintBlockItem (Stat (If ((e, s, Just s'), _))) = ("IF " ++ pPrintExp e ++ ":") : map ("  " ++) (pPrintBlockItem (Stat s)) ++ ("ELSE:" : map ("  " ++) (pPrintBlockItem (Stat s')))
pPrintBlockItem (Stat (Scope (bs, _))) = "{" : (map (\s -> "  "++s) $ concatMap pPrintBlockItem bs) ++ ["}"]
pPrintBlockItem (Decl (Declare ((v, Nothing), _))) = ["DECLARE $" ++ v]
pPrintBlockItem (Decl (Declare ((v, Just e ), _))) = ["DECLARE $" ++ v ++ " = " ++ pPrintExp e]

pPrintFunc :: FunctionDecl -> [String]
pPrintFunc (FunctionDecl ((n, s), _)) = ["FUN INT "++n++":", "  params: ()", "  body:"] ++ concatMap (map ("  "++).pPrintBlockItem) s

pPrintProg :: Program -> String
pPrintProg (Program (f, _)) = foldl (\acc s -> acc ++ s ++ "\n") "" $ pPrintFunc f
