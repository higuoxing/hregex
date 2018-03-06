{-# LANGUAGE GADTs #-}

module Regex.Automata (
     Automata    (..)   ,  -- Automata states transitions (initial state) (final states)
     Transition  (..)   ,  -- Transitions
    ) where

import Data.Char
import Data.Set (Set)
import qualified Data.Set as Set

-- Finite Automata
-- 1. Finite set of states
-- 2. Set of input chars 
-- 3. Transition functions (state --((int, Int) | epsilon)--> next state)
-- 4. Initial state
-- 5. Subset of finite set of states which are final states
data Automata state where
  Automata :: (Ord state, Show state) 
           => Set state
           -> Set (Int, Int)
           -> Set (Transition state)
           -> state
           -> Set state
           -> Automata state

instance Show state => Show (Automata state) where
  show = showAutomata

-- Transitions 
-- Edge    s0 --ch--> s1
-- Epsilon s0 ------> s1
data Transition state = Edge state (Int, Int) state
                      | Epsilon state state
  deriving (Ord, Eq)

instance Show state => Show (Transition state) where
  show = showTrans

-- disjoin a Set
  -- Input  <- [97, 99] [97, 100] [98, 108]
  -- Output -> [97, 97] [98, 99], [100, 100], [101, 108]
disjoin :: Set (Int, Int) -> Set (Int, Int)
disjoin s
  | s0 == s1  = s0
  | otherwise = disjoin s1
  where
    s0 = disjoin' s
    s1 = disjoin' s0

disjoin' :: Set (Int, Int) -> Set (Int, Int)
disjoin' s = disjoin'' s (Set.empty)

disjoin'' :: Set (Int, Int) -> Set (Int, Int) -> Set (Int, Int)
disjoin'' s0 s1
  | Set.null s0 = s1
  | otherwise = Set.insert (t0, t1) $ disjoin'' s0' s1
  where
    ((t0, t1), s0') = nextRange s0

-- to generate next range of chars
-- Time complexity: O(n*log(n))
  -- fix me!
  -- hard code map, and not so efficient, maybe update later :)
nextRange :: Set (Int, Int) -> ((Int, Int), Set (Int, Int))
nextRange s
  | Set.size s >= 2 && t0 <  t1 && t1 <  t2                         = ((t0, t1  ), s0       )
  | Set.size s >= 2 && t0 <  t1 && t1 == t2 && t2 <  t3             = ((t0, t1-1), s1       )
  | Set.size s >= 2 && t0 <  t1 && t1 == t2 && t2 == t3             = ((t0, t1-1), s2       )
  | Set.size s >= 2 && t0 <  t1 && t0 <  t2 && t1 >  t2 && t1 < t3  = ((t0, t2)  , s3       )
  | Set.size s >= 2 && t0 <  t1 && t0 == t2 && t1 >  t2 && t1 < t3  = ((t0, t1)  , s4       )
  | Set.size s >= 2 && t0 <  t1 && t1 >  t2 && t1 == t3             = ((t0, t2)  , s5       )
  | Set.size s >= 2 && t0 <  t1 && t1 >  t2 && t1 >  t3 && t2 < t3  = ((t0, t2)  , s6       )
  | Set.size s >= 2 && t0 <  t1 && t1 >  t2 && t2 == t3             = ((t0, t2-1), s7       )
  | Set.size s >= 2 && t0 == t1 && t1 <  t2 && t2 <= t3             = ((t0, t1)  , s8       )
  | Set.size s >= 2 && t0 == t1 && t1 == t2 && t2 <  t3             = ((t0, t1)  , s9       )
  | otherwise                                                       = ((t0, t1)  , Set.empty)
  where
    (t0, t1)  = Set.elemAt  0 s
    (t2, t3)  = Set.elemAt  1 s
    (s', s'') = Set.splitAt 2 s
    s0        = Set.union s'' $ Set.fromList [(t2   , t3)            ]
    s1        = Set.union s'' $ Set.fromList [(t2   , t2), (t2+1, t3)]
    s2        = Set.union s'' $ Set.fromList [(t2   , t2)            ]
    s3        = Set.union s'' $ Set.fromList [(t2+1 , t1), (t1+1, t3)]
    s4        = Set.union s'' $ Set.fromList [(t1+1 , t3)            ]
    s5        = Set.union s'' $ Set.fromList [(t1   , t3)            ]
    s6        = Set.union s'' $ Set.fromList [(t2+1 , t3), (t3+1, t1)]
    s7        = Set.union s'' $ Set.fromList [(t2   , t3), (t3+1, t1)]
    s8        = Set.union s'' $ Set.fromList [(t2   , t3)            ]
    s9        = Set.union s'' $ Set.fromList [(t2+1 , t3)            ]

-- -- get two states if an edge
-- getStates :: Transition state -> (state, state)
-- getStates (Edge s0 _ s1)  = (s0, s1)
-- getStates (Epsilon s0 s1) = (s0, s1)
-- 
-- -- get transition char
-- getEdgeChar :: Transition state -> Maybe Char
-- getEdgeChar (Edge _ c _)  = Just c
-- getEdgeChar (Epsilon _ _) = Nothing
-- 
-- -- indicates epsilon edge
-- isEpsilon :: Transition state -> Bool
-- isEpsilon (Edge _ _ _)  = False
-- isEpsilon (Epsilon _ _) = True
-- 
-- -- indicates epsilon move from state s
-- isEpsilonMoveFrom :: Eq state => Transition state -> state -> Bool
-- isEpsilonMoveFrom (Edge _ _ _) _    = False
-- isEpsilonMoveFrom (Epsilon s0 s1) s = s0 == s
-- 
-- -- one epsilon move from state s
-- epsilonMoveFrom :: (Eq state, Ord state) => Set (Transition state) -> state -> Set state
-- epsilonMoveFrom ts s = Set.foldr f Set.empty ts
--   where
--     f t set | t `isEpsilonMoveFrom` s = let (s0, s1) = getStates t in Set.insert s1 set
--             | otherwise = set
-- 
-- -- indicates transition from state s via c
-- isTransitionMoveFrom :: Eq state => Transition state -> state -> Char -> Bool
-- isTransitionMoveFrom (Epsilon _ _) s c  = False
-- isTransitionMoveFrom (Edge s0 e s1) s c = s0 == s && e == c 
-- 
-- -- one transition from state s via c
-- transitionFrom :: (Eq state, Ord state) => Set (Transition state) -> state -> Char -> Set state
-- transitionFrom ts s c = Set.foldr f Set.empty ts
--   where
--     f t set | isTransitionMoveFrom t s c = Set.insert s1 set
--             | otherwise = set
--             where
--               (s0, s1) = getStates t
-- 
-- -- epsilonClosure(s) :: Ord state => Automata state -> Set state -> Set state
-- -- set of states that transfered from state s via epsilon edge
-- epsilonClosure_T :: Ord state => Automata state -> Set state -> Set state
-- epsilonClosure_T (Automata ss_ cs ts s_ terms) ss = epsilonClosure_T' 
--                                                     (Set.filter isEpsilon ts) 
--                                                     (Set.toList ss) ss
-- 
-- epsilonClosure_T' :: Ord state => Set (Transition state) -> [state] -> Set state -> Set state
-- epsilonClosure_T' ts [] ss = ss
-- epsilonClosure_T' ts (st:stack) ss = epsilonClosure_T' ts stack' ss'
--   where
--     epm    = epsilonMoveFrom ts st
--     ss'    = Set.union ss epm
--     stack' = stack ++ Set.toList (Set.difference ss' ss)
-- 
-- -- move(T, c, s) :: Ord state => Automata state -> Set state -> Char -> Set state
-- -- set of states that transifered from state s via Char c
-- move_T :: Ord state => Automata state -> Set state -> Char -> Set state
-- move_T (Automata ss_ cs ts s_ terms) ss c = Set.foldr f Set.empty ss
--   where
--     f x set = Set.union set (transitionFrom ts x c)
-- 
-- -- transform NFA to DFA using subset construction
-- subsetConstruct
--   :: (Ord state, Eq state) => Automata state
--     -> Automata (Set state)
-- subsetConstruct nfa@(Automata ss cs ts s_ terms)
--   = subsetConstruct' nfa iniDFA iniUdss
--   where
--     iniDFA  = Automata (Set.empty) cs (Set.empty) (s_ini) (Set.empty)
--     s_ini   = epsilonClosure_T nfa (Set.fromList [s_])
--     iniUdss = Set.fromList [s_ini]
-- 
-- subsetConstruct'
--   :: (Ord state, Eq state) => Automata state
--     -> Automata (Set state) -> Set (Set state) -> Automata (Set state)
-- subsetConstruct' nfa@(Automata ss cs ts s_ terms) dfa@(Automata dss dcs dts ds_ dterms) udss
--   | Set.null udss = dfa
--   | otherwise = subsetConstruct' nfa (Automata dss' dcs' dts' ds_ dterms') udss'
--     where
--       (tset, udss'') = popDstate udss                                     -- pop T from unmarked Dstates
--       dss'           = addDstate tset dss                                 -- mark dstate
--       dcs'           = dcs                                                -- just copy dcs
--       (udss', dts')  = addTrans nfa (Set.toList dcs) tset dss' dts udss'' -- add transitions
--       dterms'        = if isTerm tset terms then addDstate tset dterms    -- add Dterms
--                        else dterms
-- 
-- -- add transition edge to dfa
-- addTrans :: (Ord state, Eq state) 
--          => Automata state                                   -- nfa
--          -> [Char]                                           -- input char set
--          -> Set state                                        -- Dstate
--          -> Set (Set state)                                  -- current Dstate
--          -> Set (Transition (Set state))                     -- DFA transition set
--          -> Set (Set state)                                  -- new Dstate
--          -> (Set (Set state), Set (Transition (Set state)))  -- 
-- addTrans nfa@(Automata ss ncs ts s_ terms)   []   d ds dts uds = (uds, dts)
-- addTrans nfa@(Automata ss ncs ts s_ terms) (c:cs) d ds dts uds = addTrans nfa cs d ds dts' uds'
--   where
--     u    = epsilonClosure_T nfa (move_T nfa d c)
--     dts' = if (Set.null u) then dts 
--            else Set.insert (Edge d c u) dts
--     uds' = if (Set.member u ds || Set.null u) then uds
--            else Set.insert u uds
-- 
-- -- indicates terminate state
-- -- isTerm :: dstate -> NFA Terms -> Bool
-- isTerm :: (Ord state, Eq state) => Set state -> Set state -> Bool
-- isTerm ds terms = not . Set.null $ Set.intersection ds terms
-- 
-- -- pop a Dstate out from umarked dstates
-- popDstate :: (Ord dstate, Eq dstate) => Set dstate -> (dstate, Set dstate)
-- popDstate dsets = (Set.elemAt 0 dsets, Set.drop 1 dsets)
-- 
-- -- add marked Dstate
-- addDstate :: (Ord dstate, Eq dstate) => dstate -> Set dstate -> Set dstate
-- addDstate = Set.insert
-- 
-- -- add unmarked Dstate
-- addUDstate :: (Ord dstate, Eq dstate) => dstate -> Set dstate -> Set dstate
-- addUDstate = Set.insert
-- 
--
-- print transition
showTrans :: Show state => Transition state -> String
showTrans (Epsilon s0 s1)         = "\n                  " 
                                 ++ show s0 
                                 ++ "--->" 
                                 ++ show s1
showTrans (Edge s0 (min, max) s1) = "\n                  " 
                                 ++ show s0 
                                 ++ "-["
                                 ++ [chr min] 
                                 ++ "-"
                                 ++ [chr max]
                                 ++ "]->"
                                 ++ show s1

-- print automata
showAutomata :: (Show state) => Automata state -> String
showAutomata (Automata ss cs ts s_ terms) 
             = "states            :" ++ 
               show ss  ++      "\n" ++
               "input chars       :" ++
               show cs  ++      "\n" ++
               "transitions       :" ++
               show ts  ++      "\n" ++
               "initial state     :" ++
               show s_  ++      "\n" ++
               "acceptable states :" ++
               show terms

