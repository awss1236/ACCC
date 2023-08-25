module CLexer where
import Data.Maybe
import Control.Monad
import Control.Applicative
import Data.Char

data Token  = Keyword String | Variable String | NumLiteral Int | Paren Char | Semicolon | Minus | Comp | LogicNeg | Add | Mult | Div deriving (Show, Eq)

newtype Lexer a = Lexer {runLexer :: String -> Maybe (a, String)}

instance Functor Lexer where
  fmap f (Lexer a) = Lexer $ \s -> do
                          (x, input') <- a s
                          Just (f x, input')

instance Applicative Lexer where
  pure a = Lexer $ \s -> Just (a, s)
  (<*>) (Lexer f) (Lexer a) = Lexer $ \s -> do
                                    (jf, in1) <- f s
                                    (ja, in2) <- a in1
                                    Just (jf ja, in2)

instance Alternative Lexer where
  empty = Lexer $ const Nothing
  (Lexer a) <|> (Lexer b) = Lexer $ \s -> a s <|> b s

spanP :: (Char -> Bool) -> Lexer String
spanP f = Lexer $ \s -> let x = span f s in if null $ fst x then Nothing else Just x

ws :: Lexer ()
ws = void $ spanP isSpace

peekCharP :: (Char -> Bool) -> Lexer Char
peekCharP f = Lexer $ \s -> if null s || f (head s) then Just (head s, s) else Nothing

predCharP :: (Char -> Bool) -> Lexer Char
predCharP f = Lexer $ \s -> if null s || f (head s) then Just (head s, tail s) else Nothing

charP :: Char -> Lexer Char
charP c = Lexer f
  where
    f [] = Nothing
    f (x:xs) = if x == c then Just (c, xs) else Nothing

stringP :: String -> Lexer String
stringP = traverse charP

semicolonP = Semicolon <$ charP ';'

minusP     = Minus     <$ charP '-'
compP      = Comp      <$ charP '~'
logicNegP  = LogicNeg  <$ charP '!'

addP       = Add      <$ charP '+'
multP      = Mult     <$ charP '*'
divP       = Div      <$ charP '/'

keywords = ["auto","break","case","char","const","continue","default","do","double","else","enum","extern","float","for","goto","if","int","long","register","return","short","signed","sizeof","static","struct","switch","typedef","union","unsigned","void","volatile","while"]
keywordP :: Lexer Token
keywordP = Keyword <$> foldr (\a b -> b <|> (stringP a <* peekCharP (not.isAlphaNum))) empty keywords

variableP :: Lexer Token
variableP = Variable <$> (liftA2 (++) (spanP (\c -> c == '_' || isAlpha c)) (spanP (\c -> c == '_' || isAlphaNum c)) <|> spanP (\c -> c == '_' || isAlpha c))

numberP :: Lexer Token
numberP = NumLiteral . read <$> spanP isDigit <* peekCharP (not . isAlpha)

parenP :: Lexer Token
parenP = Paren <$> foldr ((<|>) . charP) empty "(){}[]"

tokenP :: Lexer Token
tokenP = let nttws = keywordP <|> variableP <|> numberP <|> parenP <|> semicolonP <|> minusP <|> compP <|> logicNegP in (nttws <* ws) <|> (ws *> nttws) <|> nttws <|> (ws *> nttws <* ws)

lexC :: String -> Maybe [Token]
lexC "" = Just []
lexC s  = sequenceA $ f s where
                f :: String -> [Maybe Token]
                f "" = []
                f s  = let r = runLexer tokenP s in if isNothing r then [Nothing] else let (t, rest) = fromJust r in Just t : f rest
