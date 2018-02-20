{-# LANGUAGE GADTs #-}

import Sets

-- FA (S, Map, s0, F, Sigma)
data NFA state where
  NFA :: (Eq state, Show state) => (Set state)          -- S     : Set of states
                                -> (Set (Trans state))  -- Tran  : Transition functions (Moves)
                                -> state                -- s0    : Initial state
                                -> (Set state)          -- F     : Set of acceptable states
                                -> NFA state

instance Show state => Show (NFA state) where
  show = showNFA

data Trans state = Edge state Char state  -- edge between states
                 | Epsilon state state
  deriving (Ord, Eq, Show)

showNFA :: (Show state) => NFA state -> String
showNFA (NFA ss ts s es) = "states:          \n" ++ 
                           show ss  ++ 
                           "edges :          \n" ++
                           show ts  ++
                           "initial state:   \n" ++
                           show s   ++      "\n" ++
                           "acceptable states\n" ++
                           show es
