import Data.Complex
import Prelude hiding (pure)

data Vec a = Vec [a]

instance Functor Vec where
  fmap f (Vec lst) = Vec ([f x | x <- lst])

-- 1-show
instance Show a => Show (Vec a) where
  show (Vec a) = start : helper a
                      where start = '['
                            helper (x : ys) = ' ': show(x) ++ helper (ys)
                            helper [] = [']']
                             
-- 2-Num- + - * negate abs signum fromInteger
instance Num a => Num (Vec a) where
  (Vec lst1) + (Vec lst2) = Vec (zipWith (+) lst1 lst2)
  (Vec lst1) - (Vec lst2) = Vec (zipWith (-) lst1 lst2)
  (Vec lst1) * (Vec lst2) = Vec (zipWith (*) lst1 lst2)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger x =  pure(fromInteger x)
   
-- 3-Fractional- / fromRational
instance (Fractional a) => Fractional (Vec a) where
  (Vec lst1) / (Vec lst2) = Vec (zipWith (/) lst1 lst2)
  fromRational a =  pure(fromRational a)

-- 4-Floating-pi exp log sin cos asin acos atan sinh cosh asinh acosh atanh
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

-- 5-foldable - foldr
instance Foldable Vec where
  foldr f c (Vec a) = foldr f c a

-- 1-pure with infinty list
pure a  = Vec(repeat a)
    where repeat a = a : repeat a
    
-- 2-complex-real
realV (Vec a) = Vec([(x:+0) | x <- a])

-- 3-complex-imaginary
imagV (Vec a) = Vec([(0:+x) | x <- a])

main = do
  let v1 = Vec [1,2,3]
  let v2 = Vec [2,3,4]
  let v3 = Vec [-10,0,10]
  print $ v1 + v2
  print $ v1 - v2
  print $ v1 * v2
  print $ v1 / v2
  print(negate v1)
  print $ signum v3
  print $ abs v3
  print $ v1 + 10
  print $ v2 + 1.2
  print $ v1 + (pure $ sqrt 2)
  print $ realV v1
  print $ imagV v1
  print $ realV v1 + imagV v2
  print $ sin $ v1 * (pi / 2)
  print $ sum v1