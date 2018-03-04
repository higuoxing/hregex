module Regex.RegexParser (
    char
    ) where

import Data.Char
import Control.Monad
import Control.Applicative
import Data.Set (Set)
import qualified Data.Set as Set
import Regex.Automata (Automata)
import qualified Regex.Automata as Automata
import Regex.RegexPattern (RegExpr)
import qualified Regex.RegexPattern as RegexPattern

newtype Parser a = Parser { 
  parse :: String -> [(a, String)]  
}

-- unit operator : inject value into the result
unit :: a -> Parser a
unit a = Parser (\s -> [(a, s)])

-- bind
bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

-- combine operator
combine :: Parser a -> Parser a -> Parser a
combine p q = Parser (\s -> parse p s ++ parse q s)

-- parser that always fails
failure :: Parser a
failure = Parser (\cs -> [])

-- alternative operator
option :: Parser a -> Parser a -> Parser a
option p q = Parser $ \s ->
  case parse p s of
    []  -> parse q s
    res -> res

instance Functor Parser where
  -- fmap  :: (a -> b) -> Parser a -> Parser b
  -- (<$>) :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s])

instance Applicative Parser where
  -- pure  :: a -> Parser a
  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pure = unit
  (Parser cs1) <*> (Parser cs2) = Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

instance Monad Parser where
  -- return :: a -> Parser a
  -- (>>=)  :: Parser a -> (a -> Parser b) -> Parser b
  return = unit
  (>>=)  = bind

instance MonadPlus Parser where
  -- mzero :: Parser a
  -- mplus :: Parser a -> Parser a -> Parser a
  mzero = failure
  mplus = combine

instance Alternative Parser where
  -- empty :: Parser a
  -- (<|>) :: Parser a -> Parser a -> Parser a
  empty = failure
  (<|>) = option

-- run parser
-- runRegexParser :: Parser a -> String -> a
runRegexParser p s =
  case parse p s of
    [(res, [])] -> [(res, [])]
    [(res, rs)] -> [(res, rs)]
    cs          -> cs

-- get a item from String
item :: Parser Char
item = Parser $ \s ->
  case s of
    []     -> []
    (c:cs) -> [(c, cs)]

-- parse a determined char
char :: Char -> Parser Char
char c = satisfy (c ==)

-- parse a determined string
string :: String -> Parser String
string []     = return []
string (c:cs) = do { char c; string cs; return (c:cs) }

-- satisfy
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item `bind` \c ->
  if p c then
    unit c
  else (Parser (\cs -> []))

-- take p as token
token :: Parser a -> Parser a
token p = do { a <- p; return a }

-- reserved token
reserved :: String -> Parser String
reserved s = token (string s)

-- parse "(" and ")"
parens :: Parser a -> Parser a
parens m = do 
  reserved "("
  n <- m
  reserved ")"
  return n

-- one char from a string
oneOf :: [Char] -> Parser Char
oneOf s = satisfy (flip elem s)

{-
  - BNF Grammer of Regular Expressions
    - <RE>             ::= <union> | <simple-RE>
    - <union>          ::= <RE> "|" <simple-RE>
    - <simple-RE>      ::= <concatenation> | <basic-RE>
    - <concatenation>  ::= <simple-RE> <basic-RE>
    - <basic-RE>       ::= <star> | <plus> | <elementary-RE>
    - <star>           ::= <elementary-RE> "*"
    - <plus>           ::= <elementary-RE> "+"
    - <elementary-RE>  ::= <group> | <any> | <eos> | <char> | <set>
    - <group>          ::= "(" <RE> ")"
    - <any>            ::= "."
    - <eos>            ::= "$"
    - <char>           ::= any non metacharacter | "\" metacharacter
    - <set>            ::= <positive-set> | <negative-set>
    - <positive-set>   ::= "[" <set-items> "]"
    - <negative-set>   ::= "[^" <set-items> "]"
    - <set-items>      ::= <set-item> | <set-item> <set-items>
    - <set-item>       ::= <range> | <char>
    - <range>          ::= <char> "-" <char>
    -}

-- data RegExpr = Epsilon
--             | Literal Char
--             | Alt RegExpr RegExpr
--             | Con RegExpr RegExpr
--             | Star RegExpr

-- allowed ascii chars
chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

regex :: Parser RegexPattern.RegExpr
regex = do
  u <- re_union
  return u
  <|> do
  s <- simpleRe
  return s

re_union :: Parser RegexPattern.RegExpr
re_union = do
  s0 <- simpleRe
  reserved "|"
  s1 <- simpleRe
  s2 <- simpleReTail
  return $ RegexPattern.Alt s0 $ RegexPattern.Con s1 s2

simpleReTail :: Parser RegexPattern.RegExpr
simpleReTail = do
  reserved "|"
  s0 <- simpleRe
  s1 <- simpleReTail
  return $ RegexPattern.Con s0 s1
  <|> do
  return RegexPattern.Epsilon

simpleRe :: Parser RegexPattern.RegExpr
simpleRe = do
  c <- concatenation
  return c
  <|> do
  b <- basicRe
  return b

concatenation :: Parser RegexPattern.RegExpr
concatenation = do
  b0 <- basicRe
  b1 <- basicRe
  t  <- tailRe
  return $ RegexPattern.Con b0 $ RegexPattern.Con b1 t

tailRe :: Parser RegexPattern.RegExpr
tailRe = do
  b0 <- basicRe
  t <- tailRe
  return $ RegexPattern.Con b0 t
  <|> do
  return RegexPattern.Epsilon

-- basic-re from BNF regex
basicRe :: Parser RegexPattern.RegExpr
basicRe = do
  s <- star
  return s
  <|> do
  s <- plus
  return s
  <|> do
  s <- elementaryRe
  return s

plus :: Parser RegexPattern.RegExpr
plus = do
  e <- elementaryRe
  reserved "+"
  return $ RegexPattern.Alt e (RegexPattern.Star e)

-- star from BNF regex
star :: Parser RegexPattern.RegExpr
star = do
  e <- elementaryRe
  reserved "*"
  return $ RegexPattern.Star e

-- elementaryRe :: Parser RegexPattern
elementaryRe :: Parser RegexPattern.RegExpr
elementaryRe = do
  g <- group
  return g
  <|> do
  c <- anyChar
  return c
  <|> do
  c <- re_char
  return c
  <|> do
  c <- re_set
  return c

group :: Parser RegexPattern.RegExpr
group = do
  reserved "("
  r <- regex
  reserved ")"
  return r

-- any from BNF regex
anyChar :: Parser RegexPattern.RegExpr
anyChar = do
  reserved "."
  return $ pset $ Set.fromList chars

re_char :: Parser RegexPattern.RegExpr
re_char = do
  c <- literal
  return (RegexPattern.Literal c)

-- parse a literal from regex
literal :: Parser Char
literal = do
  c <- oneOf chars
  return c

-- parse a set or a negative set
pset :: Set Char -> RegexPattern.RegExpr
pset s
  | Set.null cs  = RegexPattern.Literal c
  | otherwise   = RegexPattern.Alt (RegexPattern.Literal c) $ pset cs
  where
    (c, cs) = pop s

-- pop a char from a Set
pop :: Set Char -> (Char, Set Char)
pop s = (Set.elemAt 0 s, Set.deleteAt 0 s)

-- set from BNF regex
re_set :: Parser RegexPattern.RegExpr
re_set = do
  ps <- postive_set
  return ps
  <|> do
  ns <- negative_set
  return ns

-- positive-set from BNF regex
postive_set :: Parser RegexPattern.RegExpr
postive_set = do
  reserved "["
  s <- set_items
  reserved "]"
  return $ pset s

-- negative-set from BNF regex
negative_set :: Parser RegexPattern.RegExpr
negative_set = do
  reserved "[^"
  s <- set_items
  reserved "]"
  return $ pset (Set.difference (Set.fromList chars) (s))

-- set-items from BNF regex
set_items :: Parser (Set Char)
set_items = do
  s <- set_item
  c <- set_items
  return $ Set.union s c
  <|> do
  s <- set_item
  return s

-- set-item from BNF regex
set_item :: Parser (Set Char)
set_item = do
  r <- range
  return $ Set.fromList r
  <|> do
  c <- literal
  return $ Set.fromList [c]

-- range from BNF regex
range :: Parser [Char]
range = do
  low  <- oneOf chars
  reserved "-"
  high <- oneOf chars
  return [low..high]

-- expr

-- exec
-- exec :: String -> RegexPattern.RegExpr
-- exec = runRegexParser expr
