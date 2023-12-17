{-
8) Dla danej liczby naturalnej n podaj najmniejszą liczbę naturalną x posiadającą m ≥ n dzielników
(łącznie z 1 i x). Przykładowo dla n = 16, x będzie równe 120 ponieważ jest pierwszą liczbą
posiadającą m = 16 dzielników.
-}

divisors :: Integral a => a -> [a]
divisors n = concatMap f (filter ((==) 0 . mod n) (takeWhile (\k -> k*k <= n) [1 .. n]))
    where f k | k < l = [k, l]
              | otherwise = [k]
              where l = div n k

fun n temp_number = 
    if (n > (length $ concatMap divisors $ [temp_number]) ) then fun n (temp_number + 1) 
    else 
        print temp_number
        --print $ concatMap divisors $ [temp_number]
    
main = do
    putStrLn "Input n"
    n <- getLine
    let number = (read n :: Int)
    fun number 1 
