module Sets (
    Set                   , -- 
    empty                 , --          Set a
    sing                  , --          a -> Set a
    memSet                , -- Ord a  => Set a -> a -> Bool
    union, inter, diff    , -- Ord a  => Set a -> Set a -> Set a
    eqSet                 , -- Eq  a  => Set a -> Set a -> Bool
    subSet                , -- Ord a  => Set a -> Set a -> Bool
    makeSet               , -- Ord a  => [a]   -> Set a
    mapSet                , -- Ord a  => (a -> b) -> Set a -> Set b
    filterSet             , --           (a -> Bool) -> Set a -> a
    foldSet               , --           (a -> a -> a) -> a -> Set a -> a
    showSet               , -- Show a => Set a -> String
    card                  , --           Set a -> Int
    flatten               , --           Set a -> [a]
    setLimit                -- Eq  a  => (Set a -> Set a) -> Set a -> Set a
    ) where

import Data.List hiding ( union )

data Set a = SetI [a]

-- empty
empty  :: Set a
empty  = SetI []

-- sing
sing   :: a -> Set a
sing x        = SetI [x]

-- memSet (member of)
memSet :: Ord a => Set a -> a -> Bool
memSet (SetI []) y = False
memSet (SetI (x:xs)) y
  | x <  y    = memSet (SetI xs) y
  | x == y    = True
  | otherwise = False

-- union
union  :: Ord a => Set a -> Set a -> Set a
union (SetI xs) (SetI ys) = SetI (uni xs ys)

uni    :: Ord a => [a] -> [a] -> [a]
uni [] ys     = ys
uni xs []     = xs
uni (x:xs) (y:ys)
  | x <  y    = x : uni xs (y:ys)
  | x == y    = x : uni xs ys
  | otherwise = y : uni (x:xs) ys

-- inter 
inter :: Ord a => Set a -> Set a -> Set a
inter (SetI xs) (SetI ys) = SetI (int xs ys)

-- int
int :: Ord a => [a] -> [a] -> [a]
int [] ys = []
int xs [] = []
int (x:xs) (y:ys)
  | x <  y    = int xs (y:ys)
  | x == y    = x : int xs ys
  | otherwise = int (x:xs) ys

-- diff
diff :: Ord a => Set a -> Set a -> Set a
diff (SetI xs) (SetI ys) = SetI (dif xs ys)

-- dif
dif :: Ord a => [a] -> [a] -> [a]
dif [] ys     = []
dif xs []     = []
dif (x:xs) (y:ys)
  | x <  y    = x : dif xs (y:ys)
  | x == y    = dif xs ys
  | otherwise = y : dif (x:xs) ys

-- subSet
subSet :: Ord a => Set a -> Set a -> Bool
subSet (SetI xs) (SetI ys) = subS xs ys

-- subS
subS :: Ord a => [a] -> [a] -> Bool
subS [] ys    = True
subS xs []    = False
subS (x:xs) (y:ys)
  | x <  y    = False
  | x == y    = subS xs ys
  | otherwise = subS (x:xs) ys

-- eqSet

