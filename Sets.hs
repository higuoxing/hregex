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
import List hiding ( union )
