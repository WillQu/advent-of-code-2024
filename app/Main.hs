module Main where

import Data.List (transpose, sort)
import System.Directory

main :: IO ()
main =
  do
    _ <- putStrLn "Day 1"
    file <- getXdgDirectory XdgData "advent-of-code-2024/input1"
    content <- readFile file
    let lists = transpose $ map words $ lines content
    let leftList = map read $ lists !! 0
    let rightList = map read $ lists !! 1
    print $ day1part1 leftList rightList
    print $ day1part2 leftList rightList

diff :: Int -> Int -> Int
diff x y = abs $ x - y

day1part1 :: [Int] -> [Int] -> Int
day1part1 leftList rightList = sum $ zipWith diff (sort leftList) (sort rightList)

day1part2 :: [Int] -> [Int] -> Int
day1part2 leftList rightList = sum $ map countSim leftList
  where
    countSim x = x * (length $ filter (== x) rightList)
