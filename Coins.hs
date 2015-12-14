module Main where

import Data.List

coins :: [Int]
coins = [2,3,5,7,9]

check :: [Int] -> Bool
check [a,b,c,d,e] = a + b * c^2 + d^3 - e == 399

solutions :: [[Int]]
solutions = filter check (permutations coins)

main :: IO ()
main = mapM_ print solutions
