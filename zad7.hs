{-
7) Dla podanej liczby n podaj, jeśli to możliwe, czwórkę pitagorejską a, b, c, d taką, że a + b + c + d=n.
Dla n = 8 jest nią odpowiednio: 1, 2, 2, 3 ponieważ 12 + 22 + 22 = 32. Dodatkowo należy narzucić
ograniczenie, aby generować tylko pierwotne czwórki pitagorejskie. Przykładowo (2, 4, 4, 6) nie jest
pierwotną czwórką pitagorejską bo wszystkie jej wartości mają wspólny dzielnik równy 2. Jeśli to
niemożliwe podaj czwórkę pitagorejską dla największego możliwego m gdzie m < n.
-}

gcda a b
    |    a==b = a
    |    a > b = gcda(a-b) b
    |    otherwise = gcda a (b-a)


triplets :: Int -> [(Int, Int, Int, Int)]
--triplets n = [(a,b,c,d) | a <- [1..n], b <- [a..n], c <- [b..n], d <- [c..n], a + b + c + d == n && (gcda(gcda (gcda a b) c) d) == 1] 
--triplets_almost n = [(a,b,c,d) | a <- [1..n], b <- [a..n], c <- [b..n], d <- [c..n], a + b + c + d > n && (gcda(gcda (gcda a b) c) d) == 1] 

triplets n = [(a,b,c,d) | a <- [1..n], b <- [a..n], c <- [b..n], d <- [c..n], a^2 + b^2 + c^2 == d^2  && (gcda(gcda (gcda a b) c) d) == 1 && (a + b + c + d == n)] 
triplets_almost n = [(a,b,c,d) | a <- [1..n], b <- [a..n], c <- [b..n], d <- [c..n], a^2 + b^2 + c^2 == d^2  && (gcda(gcda (gcda a b) c) d) == 1 && (a + b + c + d > n)] 

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
