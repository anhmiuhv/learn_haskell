module Main where

import Data.String.Strip
import Data.List
import System.Random (randomRIO)
import Debug.Trace

f = [0,0 ,1,2,3, 4]
s = [1,2,4,5,6]


randomList :: Int -> IO([Int])
randomList 0 = return []
randomList n = do
  r  <- randomRIO (1,1000)
  rs <- randomList (n-1)
  return (r:rs)


main :: IO ()
main = do
        fs <- sort <$> randomList 100000
        sn <- sort <$> randomList 1000
        print $ show $ medianOf f s
        print $ show $ median $ sort (f ++ s)

medianOf :: [Int] -> [Int] -> Float
medianOf a b= let
                len = (length a + length b)
                bol = even len
                pos = ceiling (fromIntegral len / 2) - 1
              in
                medianRecus a b pos bol

medianRecus :: [Int] -> [Int] -> Int -> Bool -> Float
medianRecus s@[a] [] x bol = getMedian s x bol
medianRecus [] s@[a] x bol = getMedian s x bol
medianRecus s a x bol = case combine s a x bol of
                          Just a -> a
                          Nothing -> let
                              meds = median s
                              meda = median a
                              leftOfS = discard s True
                              rightOfS = discard s False
                              leftOfA = discard a True
                              rightOfA = discard a False
                              in  if (length s + length a) <= 10 then getMedian (sort $ s ++ a) x bol
                                  else if meds == meda then meds
                                  else if meda < meds then
                                     medianRecus  (snd leftOfS) (snd rightOfA) (x - (fst rightOfA)) bol
                                  else medianRecus (snd rightOfS) (snd leftOfA) (x - (fst rightOfS)) bol

constrainToRange :: Int -> Int -> Int -> Int
constrainToRange ax i val = (max (min ax val) i)

lastN :: Int -> [a] -> [a]
lastN n xs = foldl (const . drop 1) xs (drop n xs)


-- true for left, false for right
discard :: [Int] -> Bool -> (Int, [Int])
discard list bol = let
                len = trace "hello" ceiling ((fromIntegral $! length list) / 2)
                in if bol then
                  (len, take len list)
                  else
                  (length list - len, lastN len list)


combine :: [Int] -> [Int] -> Int -> Bool -> Maybe Float
combine a b pos bol
          | head a >= last b = Just $ getMedian (b ++ a) pos bol
          | head b >= last a = Just $ getMedian (a ++ b) pos bol
          | otherwise = Nothing



median :: [Int] -> Float
median xs | len == 0  = -1
          | odd  len = fromIntegral $ xs !! mid
          | even len = evenMedian
                where  len = length xs
                       mid = len `div` 2 - 1
                       evenMedian :: Float
                       evenMedian =  (fromIntegral (xs !! mid) + fromIntegral (xs !! (mid+1))) / 2

getMedian :: [Int] -> Int -> Bool -> Float
getMedian list pos bool | not bool = fromIntegral $ list !! constrainToRange (length list - 1) 0 pos
                        | bool = (/) (fromIntegral (list !! constrainToRange (length list - 1) 0 pos) + fromIntegral (list !! constrainToRange (length list - 1) 0 (pos + 1))) 2