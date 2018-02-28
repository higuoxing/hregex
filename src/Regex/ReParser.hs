module Regex.ReParser (

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

