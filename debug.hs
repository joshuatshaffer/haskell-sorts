
import System.IO.Unsafe (unsafePerformIO)

debugOnEval :: String -> a -> a
debugOnEval m x = unsafePerformIO $ putStrLn m >> return x

debugFunCall :: (Show a, Show b) => String -> a -> b -> b
debugFunCall n p r = debugOnEval calMes (debugOnEval retMes $! r)
  where
    calMes = n ++ " " ++ show p ++ " called"
    retMes = n ++ " " ++ show p ++ " returned " ++ show r

badFib :: Int -> Integer
badFib k = debugFunCall "badFib" k $ badFib' k
  where
    badFib' 0 = 0
    badFib' 1 = 1
    badFib' n = badFib (n - 2) + badFib (n - 1)

goodFib :: Int -> Integer
goodFib n = fibs' !! n
  where
    fibs :: [Integer]
    fibs = 0:1:zipWith (+) (tail fibs') fibs'
    fibs' :: [Integer]
    fibs' = zipWith (debugFunCall "goodFib") [0..] fibs

main :: IO ()
main = do
  print $ badFib 4
  print $ goodFib 4
