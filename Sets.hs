module Sets (
    Set                   , --           Set a
    empty                 , --           Set a
    memberOf              , -- Ord a  => a -> Set a -> Bool
    union, inter, diff    , -- Ord a  => Set a -> Set a -> Set a
    subSet                , -- Ord a  => Set a -> Set a -> Bool
    makeSet               , -- Ord a  => [a]   -> Set a
    mapSet                , -- Ord a  => (a -> b) -> Set a -> Set b
    filterSet             , --           (a -> Bool) -> Set a -> a
    foldrSet              , --           (a -> a -> a) -> a -> Set a -> a
    foldlSet              , --           (a -> a -> a) -> a -> Set a -> a
    showSet               , -- Show a => Set a -> String
    len                   , --           Set a -> Int
    flatten               , --           Set a -> [a]
    setLimit                -- Eq  a  => (Set a -> Set a) -> Set a -> Set a
    ) where

import Data.List hiding ( union )

data Set a = Set [a]
  deriving Eq

instance (Show a) => Show (Set a) where
  show = showSet

-- empty
empty  :: Set a
empty         = Set []

-- memberOf (member of)
memberOf :: Ord a => a -> Set a -> Bool
memberOf _ (Set [])= False
memberOf x (Set (y:ys))
  | x > y = memberOf x (Set ys)
  | x < y = False
  | otherwise = True

-- union
union  :: Ord a => Set a -> Set a -> Set a
union (Set xs) (Set ys) = Set (union' xs ys)

union'    :: Ord a => [a] -> [a] -> [a]
union' [] ys     = ys
union' xs []     = xs
union' (x:xs) (y:ys)
  | x <  y    = x : union' xs (y:ys)
  | x == y    = x : union' xs ys
  | otherwise = y : union' (x:xs) ys

-- inter 
inter  :: Ord a => Set a -> Set a -> Set a
inter (Set xs) (Set ys) = Set (inter' xs ys)

-- int
inter'   :: Ord a => [a] -> [a] -> [a]
inter' [] ys  = []
inter' xs []  = []
inter' (x:xs) (y:ys)
  | x <  y    = inter' xs (y:ys)
  | x == y    = x : inter' xs ys
  | otherwise = inter' (x:xs) ys

-- diff
diff   :: Ord a => Set a -> Set a -> Set a
diff (Set xs) (Set ys) = Set (diff' xs ys)

-- dif
diff'    :: Ord a => [a] -> [a] -> [a]
diff' [] ys   = []
diff' xs []   = xs
diff' (x:xs) (y:ys)
  | x <  y    = x : diff' xs (y:ys)
  | x == y    = diff' xs ys
  | otherwise = y : diff' (x:xs) ys

-- subSet
subSet :: Ord a => Set a -> Set a -> Bool
subSet (Set xs) (Set ys) = subSet' xs ys

-- subSet'
subSet'   :: Ord a => [a] -> [a] -> Bool
subSet' [] ys = True
subSet' xs [] = False
subSet' (x:xs) (y:ys)
  | x <  y    = False
  | x == y    = subSet' xs ys
  | otherwise = subSet' (x:xs) ys

-- makeSet   
makeSet :: Ord a => [a] -> Set a
makeSet = Set . remDups . sort
  where
    remDups []  = []
    remDups [x] = [x]
    remDups (x:x':xs)
      | x < x'    = x : remDups (x':xs)
      | otherwise = remDups (x':xs)

-- mapSet    
mapSet :: Ord b => (a -> b) -> Set a -> Set b
mapSet f (Set xs) = makeSet (map f xs)

-- filterSet 
filterSet :: (a -> Bool) -> Set a -> Set a
filterSet p (Set xs) = Set (filter p xs)

-- foldrSet   
foldrSet :: (a -> a -> a) -> a -> Set a -> a
foldrSet f x (Set xs) = foldr f x xs

-- foldlSet
foldlSet :: (a -> a -> a) -> a -> Set a -> a
foldlSet f x (Set xs) = foldl f x xs

-- showSet   
showSet :: Show a => Set a -> String
showSet (Set xs) = concat (map ((++ "\n") . show) xs)

-- length      
len :: Set a -> Int
len (Set xs) = length xs

-- flatten
flatten :: Set a -> [a]
flatten (Set xs) = xs

-- setLimit  
setLimit :: Eq a => (Set a -> Set a) -> Set a -> Set a
setLimit f s
  | s == next = s
  | otherwise = setLimit f next
    where
      next = f s
