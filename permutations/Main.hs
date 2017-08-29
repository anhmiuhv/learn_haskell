{-# LANGUAGE TemplateHaskell #-}
module Main where
import Data.Foldable

slice from to xs = take (to - from + 1) (drop from xs)

takeLeftover :: [a] -> t -> [a]
takeLeftover [] _ = []
takeLeftover (x:xss) _ = xss

lastN :: Int -> [a] -> [a]
lastN n xs = foldl' takeLeftover xs (drop n xs)


permutation:: [a] -> [[a]]
permutation [] = [[]]
permutation [x] = [[x]]
permutation [x,z] = [[x,z], [z,x]]
permutation (x:xs) = spit [0..l] g
                      where l = length xs
                            g = permutation xs
                            size = length g
                            dog index el = combine (splitAt index el) x
                            combine (x,y) e = x ++ [e] ++ y
                            spit range g = dog <$> range <*> g

main :: IO ()
main = do
    d <- readLn::IO Int
    print $ take 10 (permutation [1..d])
    main
