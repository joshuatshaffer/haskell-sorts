
import System.Random (newStdGen, randomRs)
import Control.Arrow ((***))
import Data.List (partition, insert)

merge :: Ord a => ([a],[a]) -> [a]
merge (x:xs,y:ys) = if x < y then x:merge (xs,y:ys) else y:merge (x:xs,ys)
merge ([],ys) = ys
merge (xs,[]) = xs

split :: [a] -> ([a],[a])
split xs = splitAt a xs
  where a = length xs `div` 2

mergeSort :: Ord a => [a] -> [a]
mergeSort xs | length xs >= 2 = merge . (mergeSort *** mergeSort) $ split xs
             | otherwise      = xs

quickSort :: Ord a => [a] -> [a]
quickSort [] = [];
quickSort (x:xs) = foo . (quickSort *** quickSort) $ partition (<= x) xs
  where
    foo (a,b) = a ++ [x] ++ b

bubble :: Ord a => [a] -> [a]
bubble (a:b:xs) = if a < b then a:bubble (b:xs) else b:bubble (a:xs)
bubble xs = xs

bubbleSort :: Ord a => [a] -> [a]
bubbleSort xs | length xs >= 2 = (\ys-> (bubbleSort . init) ys ++ [last ys]) $ bubble xs
              | otherwise      = xs

insertionSort :: Ord a => [a] -> [a]
insertionSort = foldr insert []

main :: IO ()
main = do
  g <- newStdGen
  let xs = take 100 $ randomRs (0, 100) g :: [Int]
  print xs
  print $ bubbleSort xs
  print $ insertionSort xs
  print $ mergeSort xs
  print $ quickSort xs
