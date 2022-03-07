-- https://stackoverflow.com/questions/7958181/ranges-in-haskell-ghci
-- This range function shows Fractional Int error

-- range from to count = take count (takeWhile (<= to) (iterate (+step) from))
--                         where step = (to - from) / fromIntegral count

range _ _ 0 = []
range from to count = from : range (from + step) to (count -1)
                        where step = (to - from) / count

rd n x = [(fromIntegral . round $ y * mul) / mul | y <- x]
            where mul = 10 ^ n

absolute lst = [sqrt(x^2 + y^2) | (x,y) <- lst]

dft lst =
    let len = fromIntegral (length lst)
        imaginary = range 0 len len
        pair = zip lst imaginary

        loop [] = []
        loop(first : rest) = x_k: loop rest
            where 
            -- https://stackoverflow.com/questions/55013159/how-do-you-sum-the-individual-elements-of-a-pair-in-a-list/55013273
                  x_k = let (fsts, snds) = unzip (dft_indexwise pair) in (sum fsts, sum snds)       
                  
                  dft_indexwise [] = []
                  dft_indexwise ls = [(x* cos (2 * pi * y * first / len), -x* sin (2 * pi * y * first / len)) | (x,y) <- ls]
    in loop imaginary

main = do
let n = 64
let s = map (\t -> sin(10*2*pi*t) + sin(20*2*pi*t)/2) $ range 0 1 n
let result = map (\x -> x/n) $ absolute $ dft s
print(rd 3 s)
print(rd 2 result)