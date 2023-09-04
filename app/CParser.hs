{-#LANGUAGE LambdaCase #-}
module CParser where
import CLexer
import Control.Applicative
import Data.Maybe
import Data.Bifunctor

data UnaryOper    = UMinus | UComp | ULogicNeg    deriving (Show)
data BinaryOper   = BMult | BDiv | BAdd  | BSub | BAnd | BOr | BEqu | BNqu | BLt | BGt | BLe | BGe deriving (Show)
data Exp          = Var (Loc String) | Set (Loc (String, Exp)) | Constant (Loc Int) | UnaryAct (Loc (UnaryOper, Exp)) | BinAct (Loc (BinaryOper, Exp, Exp)) deriving (Show)

data Statement    = Expr Exp | Declare (Loc (String, Maybe Exp)) | Return (Loc Exp) deriving (Show)
data FunctionDecl = FunctionDecl (Loc (String, [Statement])) deriving (Show)
data Program      = Program (Loc FunctionDecl) deriving (Show)

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
                (parseToken (Paren '(') *> parseExp <* parseToken (Paren ')')) <|>
                (Var <$> parseVar)
      where parseInt = Constant <$> Parser (\case
                                            ((NumLiteral i, (x, y)) : rest) -> Just ((i, (x, y)), rest)
                                            _                     -> Nothing)

parseTerm  :: Parser Exp
parseTerm = let p = parseFactor in p <**> ggP p [Mult, Div] <|> p

parseAddExp = let p = parseTerm   in p <**> ggP p [Add, Minus] <|> p
parseRelExp = let p = parseAddExp in p <**> ggP p [Le, Ge, Lt, Gt] <|> p
parseEquExp = let p = parseRelExp in p <**> ggP p [Equ, Nqu] <|> p
parseAndExp = let p = parseEquExp in p <**> ggP p [And] <|> p
parseOrExp  = let p = parseAndExp in p <**> ggP p [Or] <|> p

parseExp :: Parser Exp
parseExp = ((\(i, p) e -> Set((i,e), p)) <$>parseVar <* parseToken Assign <*> parseExp) <|> parseOrExp

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

parseStatement :: Parser Statement
parseStatement = ((\(_, p) e -> Return (e, p)) <$> parseToken (Keyword "return")) <*> parseExp <* parseSemicolon
                <|> Expr <$> parseExp <* parseSemicolon
                <|> parseToken (Keyword "int")
                    *> (((\(i, p) e -> Declare ((i, e), p)) <$> parseVar)
                    <*> ((parseToken Assign *> (Just <$> parseExp)) <|> pure Nothing))
                    <* parseSemicolon

parseFunctionDecl :: Parser FunctionDecl
parseFunctionDecl = (\(s, p) ss -> FunctionDecl ((s, ss), p)) <$> parseName <*> parseStats <* parseToken (Paren '}')
                  where
                    parseName = parseToken (Keyword "int") *> parseVar <* parseToks (map Paren "(){")
                    parseStats = Parser $ \ts -> do
                                                  (s, r) <- runParser parseStatement ts
                                                  let test = runParser parseStats r
                                                  if isNothing test then Just ([s], r)
                                                  else
                                                    let (ss, r') = fromJust test
                                                    in Just (s:ss, r')
--                                <|> pure []
--                                uncomment to support empty functions

parseProgram :: Parser Program
parseProgram = (flip $ curry Program) (0, 0) <$> parseFunctionDecl

parse :: [Loc Token] -> Maybe Program
parse tk = fst <$> runParser parseProgram tk
