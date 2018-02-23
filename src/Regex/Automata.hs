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

-- indicates epsilon move from state s
isEpsilonMoveFrom :: Eq state => Transition state -> state -> Bool
isEpsilonMoveFrom (Edge _ _ _) _    = False
isEpsilonMoveFrom (Epsilon s0 s1) s = s0 == s

-- one epsilon move from state s
epsilonMoveFrom :: (Eq state, Ord state) => Set (Transition state) -> state -> Set state
epsilonMoveFrom ts s = Set.foldr f Set.empty ts
  where
    f t set | t `isEpsilonMoveFrom` s = let (s0, s1) = getStates t in Set.insert s1 set
            | otherwise = set

-- indicates transition from state s via c
isTransitionMoveFrom :: Eq state => Transition state -> state -> Char -> Bool
isTransitionMoveFrom (Epsilon _ _) s c  = False
isTransitionMoveFrom (Edge s0 e s1) s c = s0 == s && e == c 

-- one transition from state s via c
transitionFrom :: (Eq state, Ord state) => Set (Transition state) -> state -> Char -> Set state
transitionFrom ts s c = Set.foldr f Set.empty ts
  where
    f t set | isTransitionMoveFrom t s c
              = let (s0, s1) = getStates t 
                in Set.insert s1 set
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
-- set of states that transfered from state s via epsilon edge
epsilonClosure_T :: Ord state => Automata state -> Set state -> Set state
epsilonClosure_T (Automata ss_ ts s_ terms) ss = epsilonClosure_T' 
                                                 (Set.filter isEpsilon ts) 
                                                 (Set.toList ss) ss

epsilonClosure_T' :: Ord state => Set (Transition state) -> [state] -> Set state -> Set state
epsilonClosure_T' ts [] ss = ss
epsilonClosure_T' ts (st:stack) ss = epsilonClosure_T' ts stack' ss'
  where
    epm    = epsilonMoveFrom ts st
    ss'    = Set.union ss epm
    stack' = stack ++ Set.toList (Set.difference ss' ss)

-- move(T, c, s) :: Ord state => Automata state -> Set state -> Char -> Set state
-- set of states that transifered from state s via Char c
move_T :: Ord state => Automata state -> Set state -> Char -> Set state
move_T (Automata ss_ ts s_ terms) ss c = Set.foldr f Set.empty ss
  where
    f x set = Set.union set (transitionFrom ts x c)

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

