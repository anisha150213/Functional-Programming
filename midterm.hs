import Prelude hiding(sum)
import Data.Foldable (foldr)

-- 1 Haskell functions
-- 1.1 Find the max
-- 1.1.1
maxList :: [Int] -> Int
maxList [x] = x
maxList (x:y:ys) = if x >= y then maxList(x:ys) else maxList(y:ys)

-- 1.1.2
maxList' :: (a -> a -> Bool) -> [a] -> a
maxList' _ [x] = x
maxList' f (x:y:ys) = if f x y then maxList' f (x:ys) else maxList' f(y:ys)

-- 1.2 Bubble sort
-- 1.2.1
reorder :: [Int] -> (Bool, [Int])
reorder lst = case lst of [] -> (True, [])
                          lst -> (not (checked lst), result lst)
                            where
                                  checked [x] = True
                                  checked (x:y:ys) = x <= y && checked (y:ys)
                                  result [] = []
                                  result [x] = [x]
                                  result (x:y:ys) = if x <= y then x: result(y:ys) else y:result(x:ys)

-- 1.2.2
bubble_sort :: [Int] -> [Int]
bubble_sort lst = let (check,res) = reorder lst
                      helper = if (check == False) then res else bubble_sort res
                  in helper

-- 2 Type class
data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

-- 2.1
instance Functor Tree where
--   fmap:: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf a)       = Leaf (f a)
  fmap f (Node left a right) = Node (fmap f left) (f a) (fmap f right)

-- 2.2
instance Foldable Tree where
--   foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f c (Leaf a) = f a c
  foldr f c (Node left a right) = foldr f (f a (foldr f c right)) left

-- 2.3
inc a = fmap (+1) a

-- 2.4
sum a = foldr (+) 0 a

-- 2.5
toList a = foldr (:) [] a

-- 3 Monad
data Trace a = Trace (a, String)

trace :: String -> Trace ()
trace s = Trace ((), s)

fact :: Int -> Trace Int
fact x
  | x <= 1 = return 1
  | otherwise = do
      y <- fact (x - 1)
      trace ("fact " ++ show x ++ "\n")
      return (x * y)

-- 3.1
instance Functor Trace where
--   fmap :: (a -> b) -> Trace a -> Trace b
  fmap f (Trace (a, str)) = Trace ((f a), str)

-- 3.2
instance Applicative Trace where
--   pure :: a -> Trace a
  pure a =  Trace (a, " ")

--   (<*>) :: Trace (a -> b) -> Trace a -> Trace b
  Trace(a, str1) <*> Trace(b, str2) = Trace((a b), str1 <> str2)

-- 3.3
instance Monad Trace where
--   return :: a -> Trace a
  return = pure -- no need to implement
  
--   (>>=) :: Trace a -> (a -> Trace b) -> Trace b
  (Trace(a, str1)) >>= f = case f a of Trace(a', str2) -> Trace(a', str1<>str2)

main = do
  let t0 = Node (Node (Leaf 1) 2 (Leaf 3)) 4 (Node (Leaf 5) 6 (Leaf 7))
  let t1 = [2,4,1,11,9]
  let t2 = [3,1,5,0,2]
  putStrLn("Huskell function: ")
  putStrLn ("List used for max: " ++show(t1))
  putStrLn("maxList: " ++ show(maxList t1))
  putStrLn("maxList' with (>): " ++ show(maxList' (>) t1))
  putStrLn("maxList' with (<): " ++ show(maxList' (<) t1))

  putStrLn ("List used for Bubble_sort: " ++show(t2))
  putStrLn("reorder: " ++show(reorder t2))
  putStrLn("bubble_sort: " ++show(bubble_sort t2))

  putStrLn("Type Class: ")
  putStrLn ("Test Tree: " ++show(t0))
  putStrLn("inc: " ++show(inc t0))
  putStrLn("sum: " ++show(sum t0))
  putStrLn("toList: " ++show(toList t0))

  putStrLn("Monad: ")
  let Trace (x, s) = fact 10
  putStrLn s
  putStrLn $ show x