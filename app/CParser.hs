{-#LANGUAGE LambdaCase #-}
module CParser where
import CLexer
import Control.Applicative
import Data.Maybe

data UnaryOper    = UMinus | UComp | ULogicNeg    deriving (Show)
data BinaryOper  = BMult | BDiv | BAdd  | BSub | BAnd | BOr | BEqu | BNqu | BLt | BGt | BLe | BGe deriving (Show)
data Exp          = Constant Int | UnaryAct (UnaryOper, Exp) | BinAct (BinaryOper, Exp, Exp) deriving (Show)

data Statement    = Return Exp deriving (Show)
data FunctionDecl = FunctionDecl (String, Statement) deriving (Show)
data Program      = Program FunctionDecl deriving (Show)

newtype Parser a = Parser {runParser :: [Token] -> Maybe (a, [Token])}

instance Functor Parser where
  fmap f (Parser a) = Parser $ \s -> do
                          (x, input') <- a s
                          Just (f x, input')

instance Applicative Parser where
  pure a = Parser $ \s -> Just (a, s)
  (<*>) (Parser f) (Parser a) = Parser $ \s -> do
                                    (jf, in1) <- f s
                                    (ja, in2) <- a in1
                                    Just (jf ja, in2)

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser a) <|> (Parser b) = Parser $ \s -> a s <|> b s

tuParse :: Parser a -> Parser b -> Parser (a, b)
tuParse (Parser a) (Parser b) = Parser $ \s -> do
                                         (ar, as) <- a s
                                         (br, bs) <- b as
                                         Just ((ar, br), bs)

parseToken :: Token -> Parser Token
parseToken t = Parser $ \case
                       (t1:rest) -> if t == t1 then Just (t, rest) else Nothing
                       _         -> Nothing

parseToks :: [Token] -> Parser Token
parseToks [t] = parseToken t
parseToks (t:rest) = parseToken t <* parseToks rest

parseOr :: [Token] -> Parser Token
parseOr [t] = parseToken t
parseOr (t:rest) = parseToken t <|> parseOr rest

parseSemicolon :: Parser ()
parseSemicolon = Parser $ \case
                          (Semicolon : rest) -> Just ((), rest)
                          _                  -> Nothing

parseFactor :: Parser Exp
parseFactor = parseInt <|>
                ((\f -> UnaryAct (UMinus   , f)) <$> (parseToken Minus    *> parseFactor)) <|>
                ((\f -> UnaryAct (UComp    , f)) <$> (parseToken Comp     *> parseFactor)) <|>
                ((\f -> UnaryAct (ULogicNeg, f)) <$> (parseToken LogicNeg *> parseFactor)) <|>
                (parseToken (Paren '(') *> parseExp <* parseToken (Paren ')'))
      where parseInt = Constant <$> Parser (\case
                                            (NumLiteral i : rest) -> Just (i, rest)
                                            _                     -> Nothing)

parseTerm  :: Parser Exp
parseTerm = let p = parseFactor in p <**> (ggP p [Mult, Div]) <|> p

parseAddExp = let p = parseTerm   in p <**> (ggP p [Add, Minus]) <|> p
parseRelExp = let p = parseAddExp in p <**> (ggP p [Le, Ge, Lt, Gt]) <|> p
parseEquExp = let p = parseRelExp in p <**> (ggP p [Equ, Nqu]) <|> p
parseAndExp = let p = parseEquExp in p <**> (ggP p [And]) <|> p

parseExp :: Parser Exp
parseExp = let p = parseAndExp in p <**> (ggP p [Or]) <|> p

ggP :: Parser Exp -> [Token] -> Parser(Exp -> Exp)
ggP l ts = Parser (\xs -> do
                      (op', r1) <- runParser (parseOr ts) xs
                      let op = tToB op'
                      (t, r2) <- runParser l r1
                      let goofyRes = runParser (ggP l ts) r2
                      if isNothing goofyRes then Just (\x -> BinAct (op, x, t), r2)
                      else let (f, grr) = fromJust goofyRes in Just (\x -> f $ BinAct (op, x, t), grr))
      where tToB = \case
                    Add -> BAdd
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
parseStatement = p1 *> (Return <$> parseExp) <* parseSemicolon
           where p1 = Parser $ \case
                               (Keyword "return" : rest) -> Just ((), rest)
                               _                         -> Nothing

parseFunctionDecl :: Parser FunctionDecl
parseFunctionDecl = FunctionDecl <$> parse1 `tuParse` parse2
              where parse1 = let p1 = Parser $ \case
                                               (Variable ident : rest) -> Just (ident, rest)
                                               _                       -> Nothing
                                      in parseToken (Keyword "int") *> p1 <* parseToks [Paren '(', Paren ')', Paren '{']
                    parse2 = parseStatement <* parseToken (Paren '}')

parseProgram :: Parser Program
parseProgram = Program <$> parseFunctionDecl

parse :: [Token] -> Maybe Program
parse tk = fst <$> runParser parseProgram tk
