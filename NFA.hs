{-# LANGUAGE GADTs #-}

import Sets

-- FA (S, Map, s0, F, Sigma)
data NFA state where
  NFA :: (Eq state, Show state) => (Set state)               -- S     : Set of states
                                -> (Set (Transition state))  -- Tran  : Transition functions (Moves)
                                -> state                     -- s0    : Initial state
                                -> (Set state)               -- F     : Set of acceptable states
                                -> NFA state

instance Show state => Show (NFA state) where
  show = showNFA

-- Data: Transition State
data Transition state = Edge state Char state  -- edge between states
                      | Epsilon state state
  deriving (Ord, Eq, Show)

-- getStates :: Transition state -> (state, state)
getStates :: Transition state -> (state, state)
getStates (Edge s0 _ s1)  = (s0, s1)
getStates (Epsilon s0 s1) = (s0, s1)

-- isEpsilon :: Transition state -> Bool
isEpsilon :: Transition state -> Bool
isEpsilon (Edge _ _ _)  = False
isEpsilon (Epsilon _ _) = True

-- epsilonClosure(s) :: Eq state => NFA state -> state -> Set state
-- set of state that transfered from state s via epsilon edge
epsilonClosure_s :: Ord state => NFA state -> state -> Set state
epsilonClosure_s (NFA _ ts _ _) s = makeSet $ epsilonClosure_s' (flatten ts) s

-- epsilonClosure_s' :: Eq state => [Transition state] -> state -> Set state
epsilonClosure_s' :: Ord state => [Transition state] -> state -> [state]
epsilonClosure_s' [] _ = []
epsilonClosure_s' (x:xs) s
  | isEpsilon x = s : epsilonClosure_s' xs s
  | otherwise   = []
  where
    (s0, _) = getStates x

-- epsilonClosure(T) :: Eq state => NFA state -> Transition state -> Set state

-- move(T, c)        :: Eq state => NFA state -> Transition state -> state -> Set state


-- Function: showNFA :: (Show state) => NFA state -> String
showNFA :: (Show state) => NFA state -> String
showNFA (NFA ss ts s es) = "states:          \n" ++ 
                           show ss  ++ 
                           "transitions:     \n" ++
                           show ts  ++
                           "initial state:   \n" ++
                           show s   ++      "\n" ++
                           "acceptable states\n" ++
                           show es
