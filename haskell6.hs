import Data.List
--import System.Random

--Zadanie 1
zad1 = do putStrLn "Podaj dwie liczby na ktorych zostana wykonane dzialania"
          x <- getLine
          y <- getLine
          let a = read x
              b = read y
              dod = a + b
              mnoz = a * b
              odej = a - b
          putStrLn(x ++ "+" ++ y ++ "=" ++ show dod ++ " " ++ x ++ "*" ++ y ++ "=" ++ show mnoz ++ " " ++ x ++ "-" ++ y ++ "=" ++ show odej) 

--Zadanie 2
zad2 = do putStrLn "Podaj swoje imię:"
          firstName <- getLine
          putStrLn "Podaj swoje nazwisko:"
          lastName <- getLine
          putStrLn "Podaj swój numer PESEL:"
          pesel <- getLine
          let year = read (take 2 pesel) :: Int
              month = read (take 2 (drop 2 pesel)) :: Int
              day = read (take 2 (drop 4 pesel)) :: Int
              (fullYear, realMonth) = if month > 20 then (year + 2000, month - 20)
                                       else (year + 1900, month)
          if realMonth > 12 then error "Miesiac > niz 12" else do 
          if day > 31 then error "Dzien > niz 31" else do
          putStrLn (firstName ++ " " ++ lastName ++ " urodzil(a) sie " ++ show(day) ++ "." ++ show(realMonth) ++ "." ++ show(fullYear))

--Zadanie 3
nwdFunkcja :: Int -> Int -> Int
nwdFunkcja a 0 = a
nwdFunkcja 0 b = b
nwdFunkcja a 1 = 1
nwdFunkcja a b = if a==b then a else nwdFunkcja b (a `mod` b)

zad3 = do putStrLn "Podaj dwie liczby na ktorych zostana wykonane dzialania"
          x <- getLine
          y <- getLine
          let a = read x
              b = read y
              nwd = nwdFunkcja a b
              nww = (a * b) `div` nwd 
          putStrLn("NWD(" ++ x ++ "," ++ y ++ ")=" ++ show nwd ++ " " ++ "NWW(" ++ x ++ "," ++ y ++ ")=" ++ show nww) 

--Zadanie 4
zad4 = do putStrLn "podaj dwa slowa"
          x1 <- getLine
          x2 <- getLine
          if length(x1) > length(x2) then putStrLn "Pierwsze slowo jest dluzsze" 
          else if length(x1) == length(x2) then putStrLn "Oba rowno dlugie" else putStrLn "Slowo drugie dluzsze" 

--Zadanie 5
przet [] [] = ""
przet [] (x1:xs1) = "-" ++ przet [] xs1
przet (x1:xs1) [] = "-" ++ przet xs1 []
przet (x1:xs1) (x2:xs2) = if x1 == x2 then [x1] ++ przet xs1 xs2 else "-" ++ przet xs1 xs2

zad5 = do putStrLn "podaj dwa slowa"
          x1 <- getLine
          x2 <- getLine
          putStrLn(przet x1 x2)

--Zadanie 6


-- Funkcja, która przeprowadza grę
gameLoop :: Int -> Int -> IO ()
gameLoop target attempts = do
  if attempts <= 0
    then putStrLn ("Przykro mi, nie udalo sie odgadnac liczby. Liczba to: " ++ show target)
    else do
      putStrLn "Podaj liczbe z zakresu 0-99:"
      guessStr <- getLine
      let guess = read guessStr :: Int
      if guess < 0 || guess > 99
        then putStrLn "Liczba musi byc w zakresie 0-99." >> gameLoop target attempts
        else if guess == target
          then putStrLn "Gratulacje! Odgadles liczbe!"
          else do
            let hint = if guess < target then "za mala" else "za duza"
            putStrLn ("Twoja liczba jest " ++ hint ++ ". Sprobuj ponownie.")
            gameLoop target (attempts - 1)

-- Główna funkcja programu
zad6 = do
--  target <- uniformR (0, 99) -- Losuje liczbę z zakresu 0-99
  let target = 20 
  let maxAttempts = 10
  putStrLn "Witaj w grze! Masz 10 prob, aby odgadnac liczbe."
  gameLoop target maxAttempts

