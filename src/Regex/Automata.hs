{-# LANGUAGE GADTs #-}

module Regex.Automata (
    Automata           ,  -- Automata states transitions (initial state) (final states)
    Transition         ,  -- Transitions
    isEpsilon          ,  -- Indicates epsilon edge
    getStates          ,  -- Get two states of an edge
    getEdgeChar        ,  -- Get Char of an edge
    showAutomata          -- Print automata
    ) where

import Data.Set (Set)
import qualified Data.Set as Set

-- Finite Automata
-- 1. Finite set of states
-- 2. Transition functions (state --(char | epsilon)--> next state)
-- 3. Initial state
-- 4. Subset of finite set of states which are final states
data Automata state where
  Automata :: (Ord state, Show state) 
           => Set state
           -> Set (Transition state)
           -> state
           -> Set state
           -> Automata state

instance Show state => Show (Automata state) where
  show = showAutomata

-- Transitions 
data Transition state = Edge state Char state
                      | Epsilon state state
  deriving (Ord, Eq, Show)

-- indicates epsilon edge
isEpsilon :: Transition state -> Bool
isEpsilon (Edge _ _ _)  = False
isEpsilon (Epsilon _ _) = True

-- indicates epsilon edge from state s
isEpsilonFrom :: Eq state => Transition state -> state -> Bool
isEpsilonFrom (Edge _ _ _) _    = False
isEpsilonFrom (Epsilon s0 s1) s = s0 == s

-- one epsilon move from state s
epsilonMoveFrom :: (Eq state, Ord state) => Set (Transition state) -> state -> Set state
epsilonMoveFrom ts s = Set.foldr f Set.empty ts
  where
    f t set | t `isEpsilonFrom` s = let (s0, s1) = getStates t in Set.insert s1 set
            | otherwise = set

-- get two states if an edge
getStates :: Transition state -> (state, state)
getStates (Edge s0 _ s1)  = (s0, s1)
getStates (Epsilon s0 s1) = (s0, s1)

-- get transition char
getEdgeChar :: Transition state -> Maybe Char
getEdgeChar (Edge _ c _)  = Just c
getEdgeChar (Epsilon _ _) = Nothing

-- epsilonClosure(s) :: Ord state => Automata state -> Set state -> Set state
-- set of state that transfered from state s via epsilon edge
epsilonClosure_s :: Ord state => Automata state -> Set state -> Set state
epsilonClosure_s (Automata ss_ ts s_ terms) ss = epsilonClosure_s' 
                                                 (Set.filter isEpsilon ts) 
                                                 (Set.toList ss) ss

epsilonClosure_s' :: Ord state => Set (Transition state) -> [state] -> Set state -> Set state
epsilonClosure_s' ts [] ss = ss
epsilonClosure_s' ts (st:stack) ss = epsilonClosure_s' ts stack' ss'
  where
    epm    = epsilonMoveFrom ts st
    ss'    = Set.union ss epm
    stack' = stack ++ Set.toList (Set.difference ss' ss)

-- print automata
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

