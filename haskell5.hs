--Zadanie 1
f :: Eq a => [a] -> [(a, Int)]
f [] = error "Empty list"
f [a] = [(a,1)]
f (x:xs) = [(x, length(filter (==x) xs) + 1)] ++ f (filter (/=x) xs)

--Zadanie 2
--Oddzielny plik mojzbior.hs 
-- Już zrobione i działające

--Zadanie 3
data BinTree a = Puste | Wezel a (BinTree a) (BinTree a)
                  deriving (Eq, Show)

najkrotsza :: BinTree a -> Int
najkrotsza Puste = 0
najkrotsza (Wezel a l r) =  1 + min (najkrotsza l) (najkrotsza r)

--Zadanie 4
data Tree a = Empty | Node Int (Tree Int) (Tree Int) (Tree Int)
              deriving (Eq, Show)

sumTree :: Tree Int -> Int
sumTree Empty = 0
sumTree (Node a Empty Empty Empty) = a
sumTree (Node a (Node a1 l1 c1 r1) Empty Empty) = a + sumTree(Node a1 l1 c1 r1)
sumTree (Node a Empty (Node a1 l1 c1 r1) Empty) = a + sumTree(Node a1 l1 c1 r1)
sumTree (Node a Empty Empty (Node a1 l1 c1 r1)) = a + sumTree(Node a1 l1 c1 r1)
sumTree (Node a (Node a1 l1 c1 r1) (Node a2 l2 c2 r2) Empty) = a + sumTree(Node a1 l1 c1 r1) + sumTree(Node a2 l2 c2 r2)
sumTree (Node a (Node a1 l1 c1 r1) Empty (Node a2 l2 c2 r2)) = a + sumTree(Node a1 l1 c1 r1) + sumTree(Node a2 l2 c2 r2)
sumTree (Node a Empty (Node a1 l1 c1 r1) (Node a2 l2 c2 r2)) = a + sumTree(Node a1 l1 c1 r1) + sumTree(Node a2 l2 c2 r2)
sumTree (Node a (Node a1 l1 c1 r1) (Node a2 l2 c2 r2) (Node a3 l3 c3 r3)) = a + sumTree(Node a1 l1 c1 r1) + sumTree(Node a2 l2 c2 r2) + sumTree(Node a3 l3 c3 r3)

--Zadanie 5
class Krzeszewski a where 
    liczen :: a -> Int
    zwrotka :: Int -> a

data Pawel = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
    deriving (Show, Enum, Bounded)

instance Krzeszewski Pawel where
    liczen l = (case l of
        A -> 1
        B -> 2
        C -> 3
        D -> 4
        E -> 5
        F -> 6
        G -> 7
        H -> 8
        I -> 9
        J -> 10
        K -> 11
        L -> 12
        M -> 13
        N -> 14
        O -> 15
        P -> 16
        Q -> 17
        R -> 18
        S -> 19
        T -> 20
        U -> 21
        V -> 22
        W -> 23
        X -> 24
        Y -> 25
        Z -> 26)
    zwrotka a = (case a of
        1 -> A
        2 -> B
        3 -> C
        4 -> D
        5 -> E
        6 -> F
        7 -> G
        8 -> H
        9 -> I
        10 -> J
        11 -> K
        12 -> L
        13 -> M
        14 -> N
        15 -> O
        16 -> P
        17 -> Q
        18 -> R
        19 -> S
        20 -> T
        21 -> U
        22 -> V
        23 -> W
        24 -> X
        25 -> Y
        26 -> Z)

szyfrCezara :: Pawel -> Pawel
szyfrCezara a = zwrotka((liczen a + 2) `mod` 26)