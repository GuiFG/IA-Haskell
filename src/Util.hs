module Util where

import Data.Matrix
import System.Random
import Data.Random.Normal


split' :: String -> Char -> [String]
split' cs c = words $ foldr (\x xs -> if x /= c then (x:xs) else (' ':xs)) "" cs

readFloat :: String -> Float
readFloat cs = read cs :: Float

getMatrixFromString :: [String] -> Matrix Float
getMatrixFromString content = fromLists $ map (\x -> map readFloat $ split' x ',') content

subtractMatrix :: Matrix Float -> Matrix Float -> Matrix Float
subtractMatrix m1 m2 = fromList nLine nCol list
  where
    list  = zipWith (\x y -> x - y) l1 l2
    l2    = toList m2
    l1    = toList m1
    nLine = nrows m1
    nCol  = ncols m1

multScalar :: (Float -> Float) -> Matrix Float -> Matrix Float
multScalar f m = fmap f m

replaceValue :: [Float] -> Float -> Int -> [Float]
replaceValue [] _ _ = []
replaceValue xs new idx = newList ++ (snd splitList)
  where
    newList     = reverse (new : list)
    list        = tail . reverse . fst $ splitList
    splitList   = splitAt idx xs

generateRandomList :: Int -> (Float, Float) -> [Float]
generateRandomList n param = take n $ normals' param g
  where
    g = mkStdGen 137

generateRandomMatrix :: (Int, Int) -> (Float, Float) -> Matrix Float
generateRandomMatrix (l, c) param = fromList l c $ generateRandomList (l*c) param

multZipWith :: [[Float]] -> [[Float]] -> [[Float]]
multZipWith  _ []             = []
multZipWith [] _              = []
multZipWith (xs:xss) (ys:yss) = (zipWith (*) xs ys) : multZipWith xss yss


multZipWithMatrix :: Matrix Float -> Matrix Float -> Matrix Float
multZipWithMatrix m1 m2 = fromLists $ multZipWith l1 l2
  where
    l2 = toLists m2
    l1 = toLists m1

sumZipWith :: [[Float]] -> [[Float]] -> [[Float]]
sumZipWith  _ []             = []
sumZipWith [] _              = []
sumZipWith (xs:xss) (ys:yss) = (zipWith (+) xs ys) : sumZipWith xss yss


sumZipWithMatrix :: Matrix Float -> Matrix Float -> Matrix Float
sumZipWithMatrix m1 m2 = fromLists $ sumZipWith l1 l2
  where
    l2 = toLists m2
    l1 = toLists m1

indexValue :: Float -> [Float] -> Int -> Int
indexValue _ [] _ = 0
indexValue v (x:xs) c
  | x == v    = c
  | otherwise = indexValue v xs (c+1)


fromListsSafe :: [[Float]] -> Matrix Float
fromListsSafe xss
  | length xss > 0 = fromLists xss
  | otherwise      = fromLists [[]]
