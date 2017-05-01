
module Main where
  
import Sorts
import System.Random (newStdGen, randomRs)

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
