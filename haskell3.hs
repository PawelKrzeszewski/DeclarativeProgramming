import Data.List

ciag(a,b,c) = if a >= c then [] else [a] ++ ciag(a+b,b,c)

ciagBezRek(a,b,c) = [a, a+b..c] 

wycinek(n,m,lh:lt) = if n>m then [] else (if n==1 then [lh] ++ wycinek(n,m-1,lt) else wycinek(n-1,m-1,lt)) 

wycinekBezRek(n,m,l) = (take (m-n+1) . drop n) l

numerowanie(x,l:lt) = zip ([x..]) (l:lt)

iloczyn_z([], l2) = []
iloczyn_z(lh:lt, l2) = if elem lh (l2) then [lh] ++ iloczyn_z(lt,l2) else iloczyn_z(lt,l2)
 
--w sumie sortowanie
suma_z(l1,[]) = l1
suma_z(l1, lh:lt) = if elem lh (l1) then suma_z(l1,lt) else sort([lh]++suma_z(l1,lt))

roznica_z([],l1) = []
roznica_z(lh:lt, l1) = if elem lh (l1) then roznica_z(lt,l1) else sort([lh]++roznica_z(lt,l1))

powerlist [] = [[]]
powerlist (x:xs) = let rest = powerlist xs
                   in rest ++ map (x:) rest

--8. a (6/(12/(24/(8/2)))) = (6/(12/(24/4))) = 6 / (12 / 6) = 6/2 = 3
--8. b (5 == 5) && True, 3>2 && (5==5), 1>2 && 3>2
--8. c max(18,11), max(55,11), max(4,55), max(12,4), max(6,12), max(3,6)
--8. d -- kroki:
-- (6+54)/2 = 30
-- (10+30)/2 = 20
-- (4+20)/2 = 12
-- (24+12)/2 = 18
-- wynik: 18
--8. e.
-- (54+2)/2 = 56/2 = 28
-- (28+4)/2 = 32/2 = 16
-- (16+10)/2 = 26/2 = 13
-- (13+6)/2 = 19/2 = 9.5
--wynik: 9.5
--8. f. 64/4 = 16, 16/2 = 8, 8/4 = 2, 4/4 = 1
--8. g. 2*8 + 1 = 17, 2*17+2 = 36, 2*36 + 3 = 75

nalezy x xs = foldl (\acc y -> acc || y == x) False xs

mapFoldl f xs = foldl (\acc x -> acc ++ [f x]) [] xs

ostatni :: [a] -> a
ostatni = foldr1 (\_ x -> x)

pierwszy :: [a] -> a
pierwszy = foldr1 (\x _ -> x)

maxListy :: (Ord a) => [a] -> a
maxListy = foldl1 max