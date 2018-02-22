{-# LANGUAGE GADTs #-}

module Automata (
    Automata           ,
    Transition         ,
    isEpsilon          ,
    getStates          ,
    getEdgeChar        ,
    showAutomata       
    ) where

import Data.Set (Set)
import qualified Data.Set as Set

data Automata state where
  Automata :: (Ord state, Show state) 
           => Set state
           -> Set (Transition state)
           -> state
           -> Set state
           -> Automata state

instance Show state => Show (Automata state) where
  show = showAutomata

data Transition state = Edge state Char state
                      | Epsilon state state
  deriving (Ord, Eq, Show)

isEpsilon :: Transition state -> Bool
isEpsilon (Edge _ _ _)  = False
isEpsilon (Epsilon _ _) = True

getStates :: Transition state -> (state, state)
getStates (Edge s0 _ s1)  = (s0, s1)
getStates (Epsilon s0 s1) = (s0, s1)

getEdgeChar :: Transition state -> Maybe Char
getEdgeChar (Edge _ c _)  = Just c
getEdgeChar (Epsilon _ _) = Nothing

-- epsilonClosure(s) :: Ord state => Automata state -> state -> Set state
-- set of state that transfered from state s via epsilon edge
epsilonClosure_s :: Ord state => Automata state -> state -> Set state
epsilonClosure_s (Automata ss ts s_ terms) s

showAutomata :: (Show state) => Automata state -> String
showAutomata (Automata ss ts s_ terms) 
             = "states:            " ++ 
               show ss  ++      "\n" ++
               "transitions:       " ++
               show ts  ++      "\n" ++
               "initial state:     " ++
               show s_  ++      "\n" ++
               "acceptable states  " ++
               show terms
