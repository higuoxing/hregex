module Regex.ReParser (

    ) where

import Data.Char
import Control.Monad
import Control.Applicative
import Data.Set (Set)
import qualified Data.Set as Set
import Regex.Automata (Automata)
import qualified Regex.Automata as Automata

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

newtype Parser a = Parser { 
  parse :: String -> [(a, String)]  
}

unit :: a -> Parser a
unit a = Parser (\s -> [(a, s)])

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

combine :: Parser a -> Parser a -> Parser a
combine p q = Parser (\s -> parse p s ++ parse q s)

failure :: Parser a
failure = Parser (\cs -> [])

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

runRegexParser :: Parser a -> String -> a
runRegexParser p s =
  case parse p s of
    [(res, [])] -> res
    [(_  , rs)] -> error "Parser did not complete parsing"
    _           -> error "Parser error"

