module Tree where

data Tree a = Node a (Tree a) (Tree a)
            | Leaf a
            deriving (Show, Eq)

ok_tree = Node 4 (Node 2 (Leaf 1) (Leaf 3)) (Leaf 5)
one_tree = Leaf 10
small_tree = Node 6 (Leaf 5) (Leaf 7)
line_tree = Node 1 (Node 3 (Node 5 (Node 7 (Leaf 9) (Leaf 8)) (Leaf 6)) (Leaf 4)) (Leaf 2)

flatten :: Tree a -> [a]
flatten (Leaf x)       = [x]
flatten (Node x ty tz) = flatten ty ++ [x] ++ flatten tz

tmap :: (a -> b) -> Tree a -> Tree b
tmap f (Leaf x)       = Leaf (f x)
tmap f (Node x ty tz) = Node (f x) (tmap f ty) (tmap f tz)

