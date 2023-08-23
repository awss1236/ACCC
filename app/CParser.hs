{-#LANGUAGE LambdaCase #-}
module CParser where
import CLexer
import Control.Applicative

data Exp          = Constant Int | UnaryOper (Char, Exp) deriving (Show)
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

parseSemicolon :: Parser ()
parseSemicolon = Parser $ \case
                          (Semicolon : rest) -> Just ((), rest)
                          _                   -> Nothing

parseExp :: Parser Exp
parseExp = Parser $ \case
                    (NumLiteral i: rest) -> Just (Constant i, rest)
                    (Unary c: rest)      -> (\(e, t) -> (UnaryOper (c, e), t))
                                                  <$> runParser parseExp rest
                    _                    -> Nothing

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
