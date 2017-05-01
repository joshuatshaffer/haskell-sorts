
import System.Random (newStdGen, randomRs)
import Control.Arrow ((***))
import Data.List (partition, insert)

divConq :: ((c,c) -> d) -> (b -> c) -> (a -> (b,b)) -> a -> d
divConq combiner process divider = combiner . (process *** process) . divider

merge :: Ord a => ([a],[a]) -> [a]
merge (x:xs,y:ys) = if x <= y then x:merge (xs,y:ys) else y:merge (x:xs,ys)
merge ([],ys) = ys
merge (xs,[]) = xs

split :: [a] -> ([a],[a])
split xs = splitAt (length xs `div` 2) xs

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = divConq merge mergeSort split xs

mergeSort2 :: Ord a => [a] -> [a]
mergeSort2 = concat . mergeSort2' . map (:[])
  where
    mergePairs (a:b:xs) =  merge (a,b) : mergePairs xs
    mergePairs xs = xs
    mergeSort2' [] = []
    mergeSort2' [x] = [x]
    mergeSort2' xs = mergeSort2' $ mergePairs xs

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort [x] = [x]
quickSort (x:xs) = divConq (\(a,b)-> a ++ x:b) quickSort (partition (<= x)) xs

bubble :: Ord a => [a] -> [a]
bubble (a:b:xs) = if a <= b then a:bubble (b:xs) else b:bubble (a:xs)
bubble xs = xs

onInits :: ([a] -> [a]) -> [a] -> [a]
onInits f _xs = onInits' _xs []
  where
    onInits' [] ys = ys
    onInits' xs ys = onInits' (init xs') (last xs' : ys)
      where xs' = f xs

bubbleSort :: Ord a => [a] -> [a]
bubbleSort = onInits bubble

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
  print $ mergeSort2 xs
  print $ quickSort xs
