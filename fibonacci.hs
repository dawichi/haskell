-- | Calculates the first n Fibonacci numbers.
-- Examples:
--
-- >>> fibonacciList 0
-- [0]
--
-- >>> fibonacciList 1
-- [0,1]
--
-- >>> fibonacciList 10
-- [0,1,1,2,3,5,8,13,21,34,55]
fibonacciList :: Int -> [Int]
fibonacciList n = map fibonacci [0..n]


-- | Calculates the nth Fibonacci number.
-- Examples:
--
-- >>> fibonacci 0
-- 0
--
-- >>> fibonacci 1
-- 1
--
-- >>> fibonacci 10
-- 55
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)



main :: IO ()
main = do
  putStrLn "Enter a number:"
  n <- readLn
  let result = fibonacci n
  putStrLn ("The " ++ show n ++ "th Fibonacci number is " ++ show result)
  putStrLn "Let's calculate the firts N numbers of Fibonacci now"
  putStrLn "Enter a number:"
  x <- readLn
  let result2 = fibonacciList x
  putStrLn ("The first " ++ show x ++ " Fibonacci numbers are " ++ show result2)