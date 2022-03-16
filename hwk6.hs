import Data.Complex
import Data.Time
import Debug.Trace
import GHC.Base (liftA2)


-- will work same as Vec [a] since record field type is also list
newtype Vec a = Vec {runVec :: [a]} 

instance Show a => Show (Vec a) where
  show (Vec lst) = '[' : drop 1 (foldr (\e c ->' ': show e ++ c) [']'] lst) 

instance Foldable Vec where
  foldr f c (Vec a) = foldr f c a

imagV :: Num a => Vec a -> Vec (Complex a)
imagV (Vec a) = Vec $ map (0:+) a

realV :: Num a => Vec a -> Vec (Complex a)
realV (Vec a) = Vec $ map (:+0) a

--- the Vec instances of Functor, Applicative, Semigroup, Monoid, Num, Fractional, Floating
 -- 1a - Functor- fmap
instance Functor Vec where 
    fmap f (Vec lst) = Vec ([f x | x <- lst])
  
  -- 1b - Applicative- pure liftA2
instance Applicative Vec where
    pure = Vec . repeat
    liftA2 f (Vec lst1)(Vec lst2) = Vec(map (uncurry f) lst) where lst = zip lst1 lst2
  
  -- 1c - Semigroup- <>
instance Semigroup (Vec a) where
    Vec a <> Vec b = Vec(a ++ b)
  
  -- 1d - Monoid-mempty
instance Monoid (Vec a) where
    mempty = Vec []

-- 2 - Num class
instance Num a => Num (Vec a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger x =  pure(fromInteger x)

-- 2 - Floating class
instance (Floating a) => Floating (Vec a) where
  pi = pure pi
  exp = fmap exp
  log = fmap log
  sin = fmap sin
  cos = fmap cos
  asin = fmap asin
  acos = fmap acos
  atan = fmap atan
  sinh = fmap sinh
  cosh = fmap cosh
  asinh = fmap asinh
  acosh = fmap acosh
  atanh = fmap atanh
  
-- 2 - Fractional class
instance (Fractional a) => Fractional (Vec a) where
  (/) = liftA2 (/)
  fromRational a =  pure(fromRational a)

--- DFT stuff

range :: Double -> Double -> Double -> [Double]
range from to count = take (round count) (takeWhile (<= to) (iterate (+step) from))
    where step = (to - from) / count

absolute :: Vec (Complex Double) -> Vec Double
absolute (Vec lst) = Vec [sqrt(r^2 + i^2) | (r:+i) <- lst]

rd :: Int -> Vec Double -> Vec Double
rd n (Vec lst) = Vec [(fromIntegral . round $ y * mul) / mul | y <- lst]
            where mul = 10 ^ n

dft :: [Double] -> Vec (Complex Double)
dft lst = let len = fromIntegral(length lst)
              n = Vec[0..len-1]
              index = [0..len-1]
              x_r = realV(Vec lst)
              helper [] = []
              helper (first: rest) = x_k: helper rest
                where x_k = sum(x_r * exp(imagV(n * pure(-2*pi*first/len))))
          in Vec(helper index)

-- you can use this debugging helper to print intermediate fft results
-- trace_fft prints a message 'm' and the normalized value of 'x' and then returns 'x'
trace_fft :: String -> Vec (Complex Double) -> Vec (Complex Double)
trace_fft m x = trace (m ++ ": " ++ (show $ rd 2 $ fmap (/ fromIntegral (length x)) $ absolute x)) x

fft :: [Double] -> Vec (Complex Double)
fft lst  
  | len <= 16 = dft lst
  | otherwise = 
     let (even, odd) = split lst
         (even_fft, odd_fft) = (fft even, fft odd) 
        --  intermediate fft results
        --  (test_even_fft, test_odd_fft) = (trace_fft "even_fft " $ even_fft, trace_fft "odd_fft" $ odd_fft) 
         k = Vec [0..]
         const = -2 * pi / len
         img = exp( imagV (k * pure const) ) * odd_fft
        --  test_img = (trace_fft "test_imagin " img) 
         
     in
         (even_fft + img) <> (even_fft - img)
        --  to test output
        --  (test_even_fft + img) <> (test_even_fft - img)
        --  (test_even_fft) <> (test_odd_fft) <> test_img
    
  where len = fromIntegral (length lst)
        split [] = ([], [])
        split [x] = ([x], [])
        split (x:y:xs) = (x:x0, y:x1) where (x0, x1) = split xs

main = do
         let n = 2^8
         let s1 = map (\x -> sin(20*pi*x) + sin(40*pi*x)/2) $ range 0 1 n 
         -- print(rd 3 s1)

         start <- getCurrentTime
         let dft1 = fmap (/n) $ absolute $ dft s1
         print(sum $ rd 2 dft1)
         end <- getCurrentTime
         print (diffUTCTime end start)

         start2 <- getCurrentTime
         let fft1 = fmap (/n) $ absolute $ fft s1
         print(sum $ rd 2 fft1)
         end2 <- getCurrentTime
         print (diffUTCTime end2 start2)
