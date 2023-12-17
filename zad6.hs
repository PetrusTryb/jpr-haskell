{-
6) Dla podanej liczby n podaj, jeśli to możliwe, trójkę pitagorejską a, b, c taką, że a + b + c = n. Dla n =
12 jest nią odpowiednio: (3, 4, 5) ponieważ 32 + 42 = 52. Dodatkowo należy narzucić ograniczenie,
aby generować tylko pierwotne trójki pitagorejskie. Przykładowo (6, 8, 10) nie jest pierwotną trójką
pitagorejską bo wszystkie jej wartości mają wspólny dzielnik równy 2. Jeśli to niemożliwe podaj
trójkę pitagorejską dla największego możliwego m gdzie m < n.
-}

--triplets :: Int -> [(Int, Int, Int)]
--triplets d = [(a,b,c) | a <- [1..d], b <- [a..d], c <- [b..d], a^2 + b^2 == c^2]

gcda a b
    |    a==b = a
    |    a > b = gcda(a-b) b
    |    otherwise = gcda a (b-a)

triplets :: Int -> [(Int, Int, Int)]
triplets n = [(a,b,c) | a <- [1..n], b <- [a..n], c <- [b..n], a^2 + b^2 == c^2  && (gcda (gcda a b) c) == 1 && (a + b + c == n)] 
triplets_almost n = [(a,b,c) | a <- [1..n], b <- [a..n], c <- [b..n], a^2 + b^2 == c^2  && (gcda (gcda a b) c) == 1 && (a + b + c < n)] 

first :: (a, b, c) -> a
first (a, _, _) = a

second :: (a, b, c) -> b
second (_, b, _) = b

third :: (a, b, c) -> c
third (_, _, c) = c

last' :: [a] -> a
last' ys = foldl1 (\_ -> \x -> x) ys

main = do 
    putStrLn "Input n"
    number <- getLine
    let n = (read number :: Int)
    if ((length $ triplets n) == 1)
        then print $ last' $ triplets n
    else
        print $ last' $ triplets_almost n
