-- FA (S, Map, s0, F, Sigma)
-- S     : Set of states
-- Map   : Transition functions (Moves)
-- s0    : Initial state
-- F     : Set of acceptable states

import Sets

data NFA a = NFA (Set a)         -- Set of States
                 (Set (Move a))  -- Transition Functions
                 a               -- Initial State
                 (Set a)         -- Set of Acceptable States
  deriving (Show, Eq)

type Edge = Char

data Move a = Move a a Edge      -- Edge (Char)
  deriving (Eq, Ord, Show)
