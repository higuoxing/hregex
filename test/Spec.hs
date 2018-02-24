-- test bench

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Regex.Automata as Automata

main :: IO ()
main = 
  do {
       let nfa = Automata.Automata 
                          (Set.fromList [0..10])
                          (Set.fromList ['a', 'b'])
                          (Set.fromList [
                            -- (a|b)*abb
                            Automata.Epsilon 0  1  ,
                            Automata.Epsilon 0  7  ,
                            Automata.Epsilon 1  2  ,
                            Automata.Epsilon 1  4  ,
                            Automata.Edge 2 'a' 3  ,
                            Automata.Edge 4 'b' 5  ,
                            Automata.Epsilon 3  6  ,
                            Automata.Epsilon 5  6  ,
                            Automata.Epsilon 6  1  ,
                            Automata.Epsilon 6  7  ,
                            Automata.Edge 7 'a' 8  ,
                            Automata.Edge 8 'b' 9  ,
                            Automata.Edge 9 'b' 10 
                          ])
                          0
                          (Set.fromList [10])
    ; putStrLn "\ntest NFA is                 :"
    ; putStrLn (show nfa)
    ; let set_A = Automata.epsilonClosure_T nfa (Set.fromList [0])
    ; putStrLn ("\nepsilon-closure(0)            :" ++ (show set_A))
    ; let move_A_a = Automata.move_T nfa set_A 'a'
    ; putStrLn ("move(A, a)                    :" ++ (show move_A_a))
    ; let set_B = Automata.epsilonClosure_T nfa move_A_a
    ; putStrLn ("epsilon-closure(move(A, a))   :" ++ (show set_B))
  }
