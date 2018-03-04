module Regex.RegexPattern (
    RegExpr (..)
    ) where

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
    - <set-items>      ::= <range> | <char>
    - <range>          ::= <char> "-" <char>
    -}

data RegExpr = Epsilon
             | Literal Char
             | Alt RegExpr RegExpr
             | Con RegExpr RegExpr
             | Star RegExpr
             | Plus RegExpr
             | Group RegExpr

-- instance for Show
instance Show RegExpr where
  show = showRegex

-- constructNFA from Regex Expr
constructNFA :: RegExpr -> Automata Int
constructNFA (Alt re0 re1) = r_alt  (constructNFA re0) (constructNFA re1)
constructNFA (Con re0 re1) = r_con  (constructNFA re0) (constructNFA re1)
constructNFA (Star re)     = r_star (constructNFA re)
constructNFA (Literal ch)  = 
  Automata.Automata (Set.fromList [0..1]) 
                    (Set.fromList [ch])
                    (Set.fromList [Automata.Edge 0 ch 1]) 
                    0 
                    (Set.fromList [1])

-- r_star
r_star :: Automata Int -> Automata Int
r_star (Automata.Automata ss cs ts s_ terms)
  = Automata.Automata
    (ss' `Set.union` newstate)
    cs
    (ts' `Set.union` newtrans)
    0
    terms'
  where
    l        = Set.size     ss
    ss'      = Set.map      (+1)           ss
    newstate = Set.fromList [0,            (l+1)]
    ts'      = Set.map      (renum_tran 1) ts
    newtrans = Set.fromList [
      Automata.Epsilon 0 1     ,
      Automata.Epsilon 0 (l+1) ,
      Automata.Epsilon l 1     ,
      Automata.Epsilon l (l+1) ]
    terms'   = Set.fromList [(l+1)]

-- r_con
r_con :: Automata Int -> Automata Int -> Automata Int
r_con (Automata.Automata ss0 cs0 ts0 s_0 terms0)
      (Automata.Automata ss1 cs1 ts1 s_1 terms1)
  = Automata.Automata
    (ss0' `Set.union` ss1'                     )
    (cs0  `Set.union` cs1                      )
    (ts0' `Set.union` ts1'                     )
    0
    (Set.fromList [(l0 + l1 - 2)])
  where
    l0       = Set.size ss0
    l1       = Set.size ss1
    ss0'     = ss0
    ss1'     = Set.map  (+(l0-1))           ss1
    ts0'     = ts0
    ts1'     = Set.map  (renum_tran (l0-1)) ts1 
  
-- r_alt
r_alt :: Automata Int -> Automata Int -> Automata Int
r_alt (Automata.Automata ss0 cs0 ts0 s_0 terms0)
      (Automata.Automata ss1 cs1 ts1 s_1 terms1)
  = Automata.Automata
    (ss0' `Set.union` ss1' `Set.union` newstate)
    (cs0  `Set.union` cs1                      )
    (ts0' `Set.union` ts1' `Set.union` newtrans)
    0
    (Set.fromList [(l0 + l1 + 1)])
  where
    l0       = Set.size     ss0
    l1       = Set.size     ss1
    ss0'     = Set.map      (+1)                ss0
    ss1'     = Set.map      (+(l0+1))           ss1
    newstate = Set.fromList [0, (l0+l1+1)]
    ts0'     = Set.map      (renum_tran 1)      ts0
    ts1'     = Set.map      (renum_tran (l0+1)) ts1
    newtrans = Set.fromList [
      Automata.Epsilon 0       1         , 
      Automata.Epsilon 0       (l0+1)    ,
      Automata.Epsilon l0      (l0+l1+1) , 
      Automata.Epsilon (l0+l1) (l0+l1+1) ]

renum_tran :: Int -> Automata.Transition Int -> Automata.Transition Int
renum_tran i (Automata.Epsilon s0 s1) = Automata.Epsilon (s0+i) (s1+i)
renum_tran i (Automata.Edge s0 c  s1) = Automata.Edge (s0+i) c  (s1+i)

-- print the regex
showRegex :: RegExpr -> String
showRegex (Epsilon)       = "@"
showRegex (Literal c)     = [c]
showRegex (Alt re0 re1)   = showRegex re0 ++ "|" ++ showRegex re1
showRegex (Con re0 re1)   = "(" ++ showRegex re0 ++ showRegex re1 ++ ")"
showRegex (Star re)       = "(" ++ showRegex re ++ ")*"
