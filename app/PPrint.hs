module PPrint where
import Data.Maybe

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
pPrintExp (Call ((f, args), _)) = "{" ++ f ++ "(" ++ formArgs (map pPrintExp args) ++ ")}"

formArgs :: [String] -> String
formArgs [] = ""
formArgs [s] = s
formArgs (s:ss) = s++", "++formArgs ss

pPrintBlockItem :: BlockItem -> [String]
pPrintBlockItem (Stat (Expr (Just e))) = [pPrintExp e]
pPrintBlockItem (Stat (Expr Nothing))  = []
pPrintBlockItem (Stat Break) = ["BREAK"]
pPrintBlockItem (Stat Continue) = ["CONTINUE"]
pPrintBlockItem (Stat (Return (exp, _))) = ["RETURN "++pPrintExp exp]
pPrintBlockItem (Stat (If ((e, s, Nothing), _))) = ("IF " ++ pPrintExp e ++ ":") : map ("  " ++) (pPrintBlockItem (Stat s))
pPrintBlockItem (Stat (If ((e, s, Just s'), _))) = ("IF " ++ pPrintExp e ++ ":") : map ("  " ++) (pPrintBlockItem (Stat s)) ++ ("ELSE:" : map ("  " ++) (pPrintBlockItem (Stat s')))
pPrintBlockItem (Stat (While ((e, s), _))) = ("WHILE " ++ pPrintExp e) : (map ("  " ++) (pPrintBlockItem (Stat s)))
pPrintBlockItem (Stat (Do ((s, e), _))) = "DO" : (map ("  " ++) $ pPrintBlockItem (Stat s)) ++ ["WHILE " ++ pPrintExp e]
pPrintBlockItem (Stat (For ((me1, c, me2, s), _))) = [if isNothing me1 then "" else pPrintExp $ fromJust me1, "WHILE " ++ pPrintExp c ++ ":"] ++ (map ("  " ++) $ pPrintBlockItem (Stat s)) ++ (if isNothing me2 then [] else ["  " ++ (pPrintExp $ fromJust me2)])
pPrintBlockItem (Stat (ForDecl ((d, c, me, s), _))) = pPrintBlockItem (Decl d) ++ ["WHILE " ++ pPrintExp c ++ ":"] ++ (map ("  " ++) $ pPrintBlockItem (Stat s)) ++ [if isNothing me then "" else "  " ++ (pPrintExp $ fromJust me)]
pPrintBlockItem (Stat (Scope (bs, _))) = "{" : (map (\s -> "  "++s) $ concatMap pPrintBlockItem bs) ++ ["}"]
pPrintBlockItem (Decl (Declare ((v, Nothing), _))) = ["DECLARE $" ++ v]
pPrintBlockItem (Decl (Declare ((v, Just e ), _))) = ["DECLARE $" ++ v ++ " = " ++ pPrintExp e]

pPrintFunc :: FunctionDecl -> [String]
pPrintFunc (FunctionDecl ((n, args, Nothing), _)) = []
pPrintFunc (FunctionDecl ((n, args, Just bs), _)) = ["FUN INT " ++ n ++ ":", "  params: (" ++ formArgs args ++ ")", "  body:"] ++ concatMap (map ("  "++).pPrintBlockItem) bs

pPrintProg :: Program -> String
pPrintProg (Program fs) = foldl (\acc s -> acc ++ s ++ "\n") "" $ concatMap (\f -> pPrintFunc f ++ [""]) fs
