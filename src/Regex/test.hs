import Data.Set (Set)
import qualified Data.Set as Set

disjoin :: Set (Int, Int) -> Set (Int, Int) -> Set (Int, Int)
disjoin s0 s1
  | Set.null s0 = s1
  | otherwise   = Set.union s1' $ disjoin s0' s1
  where
    s0' = 
    s1' = Set.union s1 (h0, h0')
    (h0, h1) = 
