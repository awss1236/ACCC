{-#LANGUAGE LambdaCase #-}
module CParser where
import CLexer
import Control.Applicative
import Data.Maybe
import Data.Bifunctor

data UnaryOper    = UMinus | UComp | ULogicNeg    deriving (Show)
data BinaryOper   = BMult | BDiv | BAdd  | BSub | BAnd | BOr | BEqu | BNqu | BLt | BGt | BLe | BGe deriving (Show)
data Exp          = Var (Loc String) | Set (Loc (String, Exp)) |
                    Constant (Loc Int) |
                    UnaryAct (Loc (UnaryOper, Exp)) | BinAct (Loc (BinaryOper, Exp, Exp)) |
                    Tern (Loc (Exp, Exp, Exp)) |
                    Call (Loc (String, [Exp])) deriving (Show)

newtype Declare   = Declare (Loc (String, Maybe Exp)) deriving (Show)
data Statement    = Expr (Maybe Exp) | Return (Loc Exp) | Break | Continue |
                    If (Loc (Exp, Statement, Maybe Statement)) |
                    For (Loc (Maybe Exp, Exp, Maybe Exp, Statement)) | ForDecl (Loc (Declare, Exp, Maybe Exp, Statement)) |
                    While (Loc (Exp, Statement)) | Do (Loc (Statement, Exp)) |
                    Scope (Loc [BlockItem]) deriving (Show)
data BlockItem    = Stat Statement | Decl Declare deriving (Show)
data FunctionDecl = FunctionDecl (Loc (String, [String], Maybe [BlockItem])) deriving (Show)
data Program      = Program [FunctionDecl] deriving (Show)

newtype Parser a = Parser {runParser :: [Loc Token] -> Maybe (a, [Loc Token])}

instance Functor Parser where
  fmap f (Parser a) = Parser $ \s -> do
                          (x, input') <- a s
                          Just (f x, input')

instance Applicative Parser where
  pure a = Parser $ \t -> Just (a, t)
  (<*>) (Parser f) (Parser a) = Parser $ \s -> do
                                    (jf, in1) <- f s
                                    (ja, in2) <- a in1
                                    Just (jf ja, in2)

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser a) <|> (Parser b) = Parser $ \s -> a s <|> b s

tuParse :: Parser a -> Parser b -> Parser (a, b)
tuParse p1 p2 = (,) <$> p1 <*> p2

parseToken :: Token -> Parser (Loc Token)
parseToken t = Parser $ \case
                       ((t1, (x, y)):rest) -> if t == t1 then Just ((t, (x, y)), rest) else Nothing
                       _         -> Nothing

parseToks :: [Token] -> Parser (Loc Token)
parseToks [t] = parseToken t
parseToks (t:rest) = parseToken t <* parseToks rest

parseOr :: [Token] -> Parser (Loc Token)
parseOr [t] = parseToken t
parseOr (t:rest) = parseToken t <|> parseOr rest

parseVar = Parser $ \case
                     ((Variable n, p) : r) -> Just ((n, p), r)
                     _                     -> Nothing

parseSemicolon :: Parser (Loc ())
parseSemicolon = Parser $ \case
                          ((Semicolon, (x, y)) : rest) -> Just (((), (x, y)), rest)
                          _                  -> Nothing

parseFactor :: Parser Exp
parseFactor = parseInt <|>
                (((\(_, p) f -> UnaryAct ((UMinus   , f), p)) <$> parseToken Minus)    <*> parseFactor) <|>
                (((\(_, p) f -> UnaryAct ((UComp    , f), p)) <$> parseToken Comp)     <*> parseFactor) <|>
                (((\(_, p) f -> UnaryAct ((ULogicNeg, f), p)) <$> parseToken LogicNeg) <*> parseFactor) <|>
                parseCallExp <|>
                (parseToken (Paren '(') *> parseExp <* parseToken (Paren ')')) <|>
                (Var <$> parseVar)
      where parseInt = Constant <$> Parser (\case
                                            ((NumLiteral i, (x, y)) : rest) -> Just ((i, (x, y)), rest)
                                            _                     -> Nothing)

parseCallExp = (\(n, p) a -> Call ((n, a), p)) <$> parseVar
               <*> (parseToken (Paren '(') *> (argsP <|> pure []) <* parseToken (Paren ')'))
               where argsP = Parser $ \ts -> do
                                             (a, r) <- runParser parseExp ts
                                             let t = runParser (parseToken Comma *> argsP) r
                                             if isNothing t then
                                               Just ([a], r)
                                             else
                                               let (ra, r') = fromJust t
                                               in Just (a:ra, r')

parseTerm  :: Parser Exp
parseTerm = let p = parseFactor in p <**> ggP p [Mult, Div] <|> p

parseAddExp = let p = parseTerm   in p <**> ggP p [Add, Minus] <|> p
parseRelExp = let p = parseAddExp in p <**> ggP p [Le, Ge, Lt, Gt] <|> p
parseEquExp = let p = parseRelExp in p <**> ggP p [Equ, Nqu] <|> p
parseAndExp = let p = parseEquExp in p <**> ggP p [And] <|> p
parseOrExp  = let p = parseAndExp in p <**> ggP p [Or] <|> p
parseTerExp = let p = parseOrExp  in (((((\oe (_, pos) e ce -> Tern ((oe, e, ce), pos)) <$> p) <*> parseToken Quest) <*> (parseExp <* parseToken Colon)) <*> parseTerExp) <|> p

parseExp :: Parser Exp
parseExp = ((\(i, p) e -> Set((i,e), p)) <$> parseVar <* parseToken Assign <*> parseExp) <|> parseTerExp

ggP :: Parser Exp -> [Token] -> Parser(Exp -> Exp)
ggP l ts = Parser (\xs -> do
                      ((op', (a, b)), r1) <- runParser (parseOr ts) xs
                      let op = tToB op'
                      (t, r2) <- runParser l r1
                      let goofyRes = runParser (ggP l ts) r2
                      if isNothing goofyRes then Just (\x -> BinAct((op, x, t), (a, b)), r2)
                      else let (f, grr) = fromJust goofyRes in Just (\x -> f $ BinAct((op, x, t), (a, b)), grr))
      where tToB = \case
                    Add -> BAdd
                    Minus -> BSub
                    Mult -> BMult
                    Div -> BDiv
                    And -> BAnd
                    Or -> BOr
                    Equ -> BEqu
                    Nqu -> BNqu
                    Lt -> BLt
                    Gt -> BGt
                    Le -> BLe
                    Ge -> BGe

parseDeclare :: Parser Declare
parseDeclare = parseToken (Keyword "int")
                    *> (((\(i, p) e -> Declare ((i, e), p)) <$> parseVar)
                    <*> ((parseToken Assign *> (Just <$> parseExp)) <|> pure Nothing))
                    <* parseSemicolon

parseStatement :: Parser Statement
parseStatement = ((\(_, p) e -> Return (e, p)) <$> parseToken (Keyword "return")) <*> parseExp <* parseSemicolon
                <|> Expr <$> optional parseExp <* parseSemicolon
                <|> (\(_, p) e s ms -> If ((e, s, ms), p)) <$> parseToks [Keyword "if", Paren '(']
                    <*> parseExp
                    <*> (parseToken (Paren ')') *> parseStatement)
                    <*> ((parseToken (Keyword "else") *> (Just <$> parseStatement)) <|> pure Nothing)
                <|> ((\(_, p) b -> Scope (b, p)) <$> parseToken (Paren '{'))
                    <*> parseBlock
                    <* parseToken (Paren '}')
                <|> Break <$ parseToken (Keyword "break") <* parseSemicolon
                <|> Continue <$ parseToken (Keyword "continue") <* parseSemicolon
                <|> (\(_, p) e1 e2 e3 s -> For ((e1, e2, e3, s), p)) <$> parseToks [Keyword "for", Paren '(']
                    <*> (optional parseExp <* parseSemicolon)
                    <*> ((parseExp <|> pure (Constant (1, (-1, -1)))) <* parseSemicolon)
                    <*> optional parseExp
                    <*> (parseToken (Paren ')') *> parseStatement)
                <|> (\(_, p) e1 e2 e3 s -> ForDecl ((e1, e2, e3, s), p)) <$> parseToks [Keyword "for", Paren '(']
                    <*> parseDeclare
                    <*> ((parseExp <|> pure (Constant (1, (-1, -1)))) <* parseSemicolon)
                    <*> optional parseExp
                    <*> (parseToken (Paren ')') *> parseStatement)
                <|> (\(_, p) e s -> While ((e, s), p)) <$> parseToks [Keyword "while", Paren '(']
                    <*> parseExp <* parseToken (Paren ')')
                    <*> parseStatement
                <|> (\(_, p) s e -> Do ((s, e), p)) <$> parseToken (Keyword "do")
                    <*> parseStatement
                    <*> (parseToks [Keyword "while", Paren '('] *> parseExp <* parseToks [Paren ')', Semicolon])

parseBlockItem :: Parser BlockItem
parseBlockItem = Stat <$> parseStatement <|> Decl <$> parseDeclare

parseBlock :: Parser [BlockItem]
parseBlock = Parser (\ts -> do
                            (i, r) <- runParser parseBlockItem ts
                            let check = runParser parseBlock r
                            if isNothing check then
                              Just ([i], r)
                            else
                              let (is, r') = fromJust check
                              in Just (i:is, r'))
            <|> pure []

parseFunctionDecl :: Parser FunctionDecl
parseFunctionDecl = (\(_, p) (n, _) a b -> FunctionDecl ((n, a, b), p)) <$> parseToken (Keyword "int")
                    <*> parseVar
                    <*> (parseToken (Paren '(') *> (argumentP <|> pure []) <* parseToken (Paren ')'))
                    <*> ((Just <$> (parseToken (Paren '{') *> parseBlock <* parseToken (Paren '}')))
                        <|> (Nothing <$ parseSemicolon))
                  where argumentP = Parser $ \ts -> do
                                                     ((a, _), r) <- runParser (parseToken (Keyword "int") *> parseVar) ts
                                                     let t = runParser (parseToken Comma *> argumentP) r
                                                     if isNothing t then
                                                       Just ([a], r)
                                                     else
                                                       let (ra, r') = fromJust t
                                                       in Just (a:ra, r')

parseProgram :: Parser Program
parseProgram = Program <$> fsP
                  where fsP = Parser (\ts -> do
                               (f, r) <- runParser parseFunctionDecl ts
                               let t = runParser fsP r
                               if isNothing t then
                                 Just ([f], r)
                               else
                                 let (rf, r') = fromJust t
                                 in Just (f:rf, r'))

parse :: [Loc Token] -> Maybe Program
parse tk = fst <$> runParser parseProgram tk
