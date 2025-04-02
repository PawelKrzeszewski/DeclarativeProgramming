import Data.List

--Zadanie 1
--Wyrażenia lambda


--Zadanie 2
data Moto = Audi|Citroen|Ford|Toyota|Volvo
             deriving (Show)

type Kraj = String

markaPanstwo :: Kraj -> Moto
markaPanstwo m = case m of
              "Niemcy" -> Audi
              "Francja" -> Citroen
              "USA" -> Ford
              "Japonia" -> Toyota
              "Szwecja" -> Volvo

markaPredkosc :: Moto -> Int
markaPredkosc m = case m of
                  Audi -> 340
                  Citroen -> 220
                  Ford -> 360
                  Toyota -> 320
                  Volvo -> 280

--Zadanie 3
data Uczelnia = UAM|UMK|UG|UW|UJ 
                 deriving (Show)


rM :: Uczelnia -> (Int, String)
rM a = case a of
       UAM -> (1990,"Poznan")
       UMK -> (1980,"Torun")
       UG -> (1960,"Gdansk")
       UW -> (1910,"Warszawa")
       UJ -> (1900,"Krakow")

--Zadanie 4
data Tree a = Empty | Node a (Tree a) (Tree a)
              deriving (Eq, Show)

ta :: Tree Int
ta = Node 1 (Node 2 (Node 4 Empty Empty) (Node 5 Empty (Node 8 Empty Empty))) (Node 3 (Node 6 Empty (Node 9 Empty Empty)) (Node 7 Empty Empty))

tb :: Tree Char
tb = Node 'a' (Node 'b' Empty (Node 'd' (Node 'f' Empty Empty) Empty)) (Node 'c' (Node 'e' Empty (Node 'g' Empty Empty)) Empty)

preorder :: Tree a -> [a]
preorder Empty = []
preorder (Node a l r) = [a] ++ preorder l ++ preorder r

inorder :: Tree a -> [a]
inorder Empty = []
inorder (Node a l r) = inorder l ++ [a] ++ inorder r

postorder :: Tree a -> [a]
postorder Empty = []
postorder (Node a l r) = postorder l ++ postorder r ++ [a]

--Zadanie 5a
treeMemberA :: Eq a => Tree a -> a -> Bool
treeMemberA (Node a l r) x = elem x (inorder (Node a l r))

--Zadanie 5b
treeMemberB :: Eq a => Tree a -> a -> Bool
treeMemberB Empty x = False
treeMemberB (Node a l r) x
  | a == x    = True
  | otherwise = treeMemberB l x || treeMemberB r x

--Zadanie 6
poddrzewo :: Eq a => Tree a -> Tree a -> Bool
poddrzewo Empty _ = True
poddrzewo _ Empty = False
poddrzewo (Node a1 l1 r1) (Node a2 l2 r2)
  = Node a1 l1 r1 == Node a2 l2 r2 || poddrzewo (Node a1 l1 r1) l2 || poddrzewo (Node a1 l1 r1) r2
  
--Zadanie 7
-- Funkcja pomocnicza do przeglądania poziomów drzewa
bfs :: Eq a => [Tree a] -> [a]
bfs [] = []
bfs xs = map root xs ++ bfs (concatMap children xs)
  where
    root (Node a _ _) = a
    root Empty = error "Korzen pusty"
    children (Node _ l r) = filter (/= Empty) [l, r]
    children Empty = []

-- Funkcja do wypisywania drzewa poziomo
poziomo :: Eq a => Tree a -> [a]
poziomo t = bfs [t]