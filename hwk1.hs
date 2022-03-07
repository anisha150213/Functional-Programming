-- https://gist.github.com/mikehaertl/3258427

toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0    = []
    | otherwise     = toDigits (div n 10) ++ [mod n 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse(toDigits n)

-- https://stackoverflow.com/questions/19867491/double-every-other-element-of-list-from-right-in-haskell/45703015
doubleEveryOther xs =  zipWith (curry (\x -> if even (fst x) then snd x * 2 else snd x)) (reverse [1..n]) xs
                        where n = length xs

-- concatMap takes a function and list and works following way
-- concatMap (\i -> [-i,i]) [1,2,3]
-- [-1,1,-2,2,-3,3]
-- https://hoogle.haskell.org/?hoogle=concatMap
-- sumDigits :: [Integer] -> [Integer]
-- sumDigits xs =  sum( (concat. map toDigits) xs)

sumDigits :: [Integer] -> Integer
sumDigits xs = sum (concatMap toDigits xs)

validate :: Integer -> Bool
validate n = mod (sumDigits (doubleEveryOther (toDigits n))) 10 == 0

main = do
    putStrLn $ show $ validate 4012888888881881
    putStrLn $ show $ validate (4012888888881882)
