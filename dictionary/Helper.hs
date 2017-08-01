{-# LANGUAGE OverloadedStrings,RecordWildCards #-}

module Helper where
import qualified Data.Trie as Tr
import Data.Text as T
import Data.Text.Encoding
import qualified Data.List as L

sort :: [Text] -> [Text]
sort (x:xs) = lesser ++ x:greater
        where 
            lesser = sort [y | y <- xs, T.length y < T.length x]
            greater = sort [y | y <- xs, T.length y >= T.length x]
sort _ = []

conso::[Text]
conso = sort ["b", "d", "dʒ", "ð", "f", "ɡ", "h", "hw", "j", "k", "l", "m", "n", "ŋ", "p", "r", "s", "ʃ", "t", "tʃ", "θ", "v", "w", "z", "ʒ", "x", "ʔ", "ɒ̃", "æ̃"]
vowel::[Text]
vowel = sort [ "ɑː", "ɒ", "æ", "aɪ", "aɪ.ər", "aʊ", "aʊ.ər", "ɛ", "eɪ", "eɪ.ər", "ɪ", "iː", "iː.ər", "ɔː", "ɔː.ər", "ɔɪ", "ɔɪ.ər", "oʊ", "oʊ.ər", "ʊ", "uː", "uː.ər", "juː", "juː.ər", "ʌ", "ʌr", "ə", "əl", "ən"]
sorted = sort $ conso ++ vowel

longest_conso = L.foldl' max (-2)  (T.length <$> conso)
longest_vowel = L.foldl' max (-2) (T.length <$> vowel)
a = Tr.fromList $ L.zip (encodeUtf8 <$> sorted) sorted
c = Tr.fromList $ L.zip (encodeUtf8 <$> conso) conso
v = Tr.fromList $ L.zip (encodeUtf8 <$> vowel) vowel

matchWithText:: Tr.Trie a -> Text -> Maybe (Text, Text)
matchWithText tr t = let result = Tr.match tr $ encodeUtf8 t
              in case result of 
                Nothing -> Nothing
                Just (prefix,_,remain) -> Just (decodeUtf8 prefix, decodeUtf8 remain)

hyphenated::Text -> [Text]
hyphenated "" = []
hyphenated x =  let result = matchWithText a x
                in case result of
                    Nothing -> []
                    Just (t, ts) -> t : hyphenated ts

determine:: (Int, Int, Int) -> [[Text]] -> IO ()
determine (pos1, pos2, pos3) pr = let syll1 = pr !! 0 !! (pos1 - 1)
                                      syll2 = pr !! 1 !! (pos2 - 1)
                                      syll3 = pr !! 2 !! (pos3 - 1)
                                  in result syll1 syll2 syll3
                                    where 
                                        result ::Text -> Text -> Text -> IO()
                                        result syll1 syll2 syll3
                                            | syll1 == syll2 = putStrLn "c"
                                            | syll2 == syll3 = putStrLn "a"
                                            | syll1 == syll3 = putStrLn "b"
                                            | otherwise = putStrLn "wrong input"
                        

