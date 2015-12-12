module Main where

data Room = Number Int | Op Op
  deriving Show
data Op = Add | Sub | Mul
  deriving Show

data Node = Node Room [Node] Bool

a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4 :: Node

a1 = Node (Op Mul)    [   a2,b1] False
a2 = Node (Number 4)  [a1,a3,b2] False
a3 = Node (Op Add)   [a2,   b3] False
a4 = Node (Number 22) [a3,   b4] False

b1 = Node (Number 8) [a1,   b2,c1] False
b2 = Node (Op Mul)   [a2,b1,b3,c2] False
b3 = Node (Number 4) [a3,b2,b4,c3] False
b4 = Node (Op Sub)   [   b3   ,c4] False

c1 = Node (Op Sub)    [b1,   c2,d1] False
c2 = Node (Number 11) [b2,c1,c3,d2] False
c3 = Node (Op Sub)    [b3,c2,c4,d3] False
c4 = Node (Number 9)  [b4,c3,   d4] False

d1 = Node (Number 1)  [c1,   d2] True
d2 = Node (Op Mul   ) [c2,d1,d3] False
d3 = Node (Number 18) [c3,d2,d4] False
d4 = Node (Op Mul  )  [c4,d3   ] False

walk ::
  Int      {- ^ fuel -} ->
  [Room]   {- ^ rooms visited so far -} ->
  Node     {- ^ current location in the graph -} ->
  [[Room]] {- ^ valid paths -}
walk 0 acc (Node x _ True) = [reverse (x:acc)]
walk 0 _   (Node _ _ False) = []
walk fuel acc (Node x xs _) =
  do next <- xs
     walk (fuel-1) (x:acc) next

eval :: [Room] -> Int
eval [Number x]=x
eval (Number x : Op op : Number y : xs) =
  eval (Number x':xs)
  where
  x' = case op of
         Add -> x+y
         Mul -> x*y
         Sub -> x-y
eval other = error (show other)

main :: IO ()
main = print (head search)

-- | All the paths that reach the goal with a value of 30 in order of path
-- length.
search :: [[Room]]
search =
  [ path
  | fuel <- [6,8..]
  , path <- walk fuel [] a4
  , eval path == 30
  ]
