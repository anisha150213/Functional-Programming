import Data.Time

range from to count = take (round count) (takeWhile (<= to) (iterate (+step) from))
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
                  dft_indexwise ls = [(x * cos (2 * pi * y * first / len), -x * sin (2 * pi * y * first / len)) | (x,y) <- ls]
    in loop imaginary

split [] = ([], [])
split [x] = ([x], [])
split (x:y:xs) = (x:x0, y:x1) where (x0, x1) = split xs

fft lst
    | length lst <= 16 = dft lst
    | otherwise = let len = fromIntegral (length lst)
                      (even, odd) = split lst
                      even_fft = fft even
                      odd_fft = fft odd
                      pair = zip3 even_fft odd_fft [0..]                   
                      fft_merge = let (y_k, y_k') = unzip ([helper x | x <- pair]) in y_k ++ y_k'
                      helper ((even_a, even_b), (odd_c, odd_d), k) =
                        let const = -2 * pi * fromIntegral k/ len
                            u_k = cos const
                            v_k = sin const

                            y_a_pos = even_a + u_k * odd_c - v_k * odd_d
                            y_a_neg = even_a - u_k * odd_c + v_k * odd_d
                            y_b_pos = even_b + u_k * odd_d + v_k * odd_c
                            y_b_neg = even_b - u_k * odd_d - v_k * odd_c
                            
                        in ((y_a_pos, y_b_pos), (y_a_neg, y_b_neg))     

    in fft_merge
  
main = do
    let n = 2^8
    let s1 = map (\x -> sin(20*pi*x) + sin(40*pi*x)/2) $ range 0 1 n
    print(rd 3 s1)
    let result = map (/ n) $ absolute $ dft s1
    -- print(split(rd 2 result))
    start <- getCurrentTime
    let dft1 = map (/ n) $ absolute $ dft s1
    print(rd 2 dft1)
    end <- getCurrentTime
    print (diffUTCTime end start)
    start2 <- getCurrentTime
    let fft2 = map (/ n) $ absolute $ fft s1
    print(rd 2 fft2)
    end2 <- getCurrentTime
    print (diffUTCTime end2 start2)

