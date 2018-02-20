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

-- epsilonClosure(s) :: Ord state => NFA state -> state -> Set state
-- set of state that transfered from state s via epsilon edge
epsilonClosure_s :: Ord state => NFA state -> state -> Set state
epsilonClosure_s (NFA _ ts _ _) s = makeSet $ epsilonClosure_s' (flatten ts) s

epsilonClosure_s' :: Ord state => [Transition state] -> state -> [state]
epsilonClosure_s'  []  _   = []
epsilonClosure_s' (x:xs) s
  | isEpsilon x && s0 == s = s1 : epsilonClosure_s' xs s
  | otherwise              = epsilonClosure_s' xs s
  where
    (s0, s1) = getStates x

-- epsilonClosure(T) :: Ord state => NFA state -> Set state -> Set state
-- set of state that transfered from a set of states T via epsilon edge
epsilonClosure_t :: Ord state => NFA state -> Set state -> Set state
epsilonClosure_t (NFA _ ts _ _) ss = makeSet $ epsilonClosure_t' (flatten ts) (flatten ss)

epsilonClosure_t' :: Ord state => [Transition state] -> [state] -> [state]
epsilonClosure_t' [] _ = []
epsilonClosure_t' _ [] = []
epsilonClosure_t' ts ss = concat [epsilonClosure_s' ts s | s <- ss]

-- move(T, a) :: Ord state => NFA state -> Transition state -> Set state
-- set of state that transfered from state s via normal 'a' edge
move_t :: Ord state => NFA state -> state -> Char -> Set state
move_t (NFA _ ts _ _) s c = makeSet $ move_t' (flatten ts) s c

move_t' :: Eq state => [Transition state] -> state -> Char -> [state]
move_t' [] _ _ = []
move_t' (x:xs) s c
  | not (isEpsilon x) && s == s0 && edgeCh == Just c = s1: move_t' xs s c
  | otherwise = move_t' xs s c
  where
    (s0, s1) = getStates x
    edgeCh   = getEdgeChar x

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
