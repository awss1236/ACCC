module CLexer where
import Data.Char
import Data.Maybe
import Data.Bifunctor
import Control.Monad
import Control.Applicative

data Token = Keyword String | Variable String | NumLiteral Int | Paren Char | Semicolon
              | Minus | Comp | LogicNeg
              | Add | Mult | Div | And | Or | Equ | Nqu | Lt | Gt | Le | Ge deriving (Show, Eq)

newtype Lexer a = Lexer {runLexer :: String -> Maybe ((a, Int, Int), String)}

instance Functor Lexer where
  fmap f (Lexer a) = Lexer $ \s -> do
                          ((x, p, l), input') <- a s
                          Just ((f x, p, l), input')

instance Applicative Lexer where
  pure a = Lexer $ \s -> Just ((a, 0, 0), s)
  (<*>) (Lexer f) (Lexer a) = Lexer $ \s -> do
                                    ((jf, p1, l1), in1) <- f s
                                    ((ja, p2, l2), in2) <- a in1
                                    Just ((jf ja, if l2 == 0 then p1+p2 else p2, l1+l2), in2)

instance Alternative Lexer where
  empty = Lexer $ const Nothing
  (Lexer a) <|> (Lexer b) = Lexer $ \s -> a s <|> b s


spanL :: (Char -> Bool) -> Lexer String
spanL f = Lexer $ \s -> let x = fst $ span f s in if null x then Nothing else runLexer (stringL x) s

ws :: Lexer ()
ws = void $ spanL isSpace

peekCharL :: (Char -> Bool) -> Lexer Char
peekCharL f = Lexer $ \s -> if (not $ null s) && f (head s) then Just $ bimap (\(c, _, _) -> (c, 0, 0)) (\_ -> s) (fromJust $ runLexer (charL $ head s) s) else Nothing

predCharL :: (Char -> Bool) -> Lexer Char
predCharL f = Lexer $ \s -> if (not $ null s) && f (head s) then runLexer (charL $ head s) s else Nothing

charL :: Char -> Lexer Char
charL c = Lexer f
  where
    f [] = Nothing
    f (x:xs) = if x == c then Just (if c == '\n' then (c, 0, 1) else (c, 1, 0), xs) else Nothing

stringL :: String -> Lexer String
stringL = traverse charL

semicolonL = Semicolon <$ charL ';'

minusL     = Minus     <$ charL '-'
compL      = Comp      <$ charL '~'
logicNegL  = LogicNeg  <$ charL '!'

addL       = Add      <$ charL '+'
multL      = Mult     <$ charL '*'
divL       = Div      <$ charL '/'
ltL        = Lt       <$ charL '<'
gtL        = Gt       <$ charL '>'
andL       = And      <$ charL '&' <* charL '&'
orL        = Or       <$ charL '|' <* charL '|'
equL       = Equ      <$ charL '=' <* charL '='
nquL       = Nqu      <$ charL '!' <* charL '='
leL        = Le       <$ charL '<' <* charL '='
geL        = Ge       <$ charL '>' <* charL '='

keywords = ["auto","break","case","char","const","continue","default","do","double","else","enum","extern","float","for","goto","if","int","long","register","return","short","signed","sizeof","static","struct","switch","typedef","union","unsigned","void","volatile","while"]
keywordL :: Lexer Token
keywordL = Keyword <$> foldr (\a b -> b <|> (stringL a <* peekCharL (not.isAlphaNum))) empty keywords

variableL :: Lexer Token
variableL = Variable <$> (liftA2 (++) (spanL (\c -> c == '_' || isAlpha c)) (spanL (\c -> c == '_' || isAlphaNum c)) <|> spanL (\c -> c == '_' || isAlpha c))

numberL :: Lexer Token
numberL = NumLiteral . read <$> spanL isDigit <* peekCharL (not . isAlpha)

parenL :: Lexer Token
parenL = Paren <$> foldr ((<|>) . charL) empty "(){}[]"

tokenL :: Lexer Token
tokenL = let nttws = keywordL <|> variableL <|> numberL <|> parenL <|> semicolonL
                 <|> minusL <|> compL <|> logicNegL
                 <|> addL <|> multL <|> divL <|> ltL <|> gtL <|> andL <|> orL <|> equL <|> nquL <|> leL <|> geL
            in (ws *> nttws <* ws) <|> (ws *> nttws) <|> nttws
{-
lexC :: String -> Maybe [Token]
lexC "" = Just []
lexC s  = sequenceA $ f s where
                f :: String -> [Maybe (Token, Int, Int)]
                f "" = []
                f s  = let r = runLexer tokenL s in if isNothing r then [Nothing] else let (t, rest) = fromJust r in Just t : f rest-}
