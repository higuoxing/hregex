{-# LANGUAGE GADTs #-}

import Sets

-- FA (S, Map, s0, F, Sigma)
data Automata state where
  Automata :: (Eq state, Show state) => (Set state)               -- S     : Set of states
                                -> (Set (Transition state))  -- Tran  : Transition functions (Moves)
                                -> state                     -- s0    : Initial state
                                -> (Set state)               -- F     : Set of acceptable states
                                -> Automata state

instance Show state => Show (Automata state) where
  show = showAutomata

-- Data: Transition State
data Transition state = Edge state Char state  -- edge between states
                      | Epsilon state state
  deriving (Ord, Eq, Show)

-- get states of a transition or a epsilon edge
getStates :: Transition state -> (state, state)
getStates (Edge s0 _ s1)  = (s0, s1)
getStates (Epsilon s0 s1) = (s0, s1)

-- get the char of the normal edge
getEdgeChar :: Transition state -> Maybe Char
getEdgeChar (Edge _ c _)  = Just c
getEdgeChar (Epsilon _ _) = Nothing

-- indicate if it is a epsilon edge
isEpsilon :: Transition state -> Bool
isEpsilon (Edge _ _ _)  = False
isEpsilon (Epsilon _ _) = True

-- epsilonClosure(s) :: Ord state => Automata state -> state -> Set state
-- set of state that transfered from state s via epsilon edge

-- epsilonClosure(T) :: Ord state => Automata state -> Set state -> Set state
-- set of state that transfered from a set of states T via epsilon edge


-- move(T, a) :: Ord state => Automata state -> Transition state -> Set state
-- set of state that transfered from state s via normal 'a' edge


-- Function: showAutomata :: (Show state) => Automata state -> String
showAutomata :: (Show state) => Automata state -> String
showAutomata (Automata ss ts s es) = "states:          \n" ++ 
                           show ss  ++ 
                           "transitions:     \n" ++
                           show ts  ++
                           "initial state:   \n" ++
                           show s   ++      "\n" ++
                           "acceptable states\n" ++
                           show es
