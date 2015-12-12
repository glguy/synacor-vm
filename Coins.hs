module Main where

import Data.List

coins = [2,3,5,7,9]
check [a,b,c,d,e] = a + b * c^2 + d^3 - e == 399
main = print (find check (permutations coins))
