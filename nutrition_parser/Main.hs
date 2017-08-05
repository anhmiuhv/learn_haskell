
module Main where

import Text.Regex.PCRE.String
import Text.Regex.PCRE
import Data.Maybe
main :: IO ()
main = do
	putStrLn "Type the name of the ocr files"
	filename <- getLine
	s <- readFile filename
	cs <- search patt s
	g <- search patt2 s
	print $ zip cs g
	
patt = makeRegexOpts compCaseless execBlank "(total fat|trans fat|sat fat|Sugars|proteins|carb)" :: Regex
patt2 = makeRegexOpts compCaseless execBlank "\\d+\\s*(mg|g|%)" :: Regex

search :: Regex -> String -> IO [String]
search _ "" = return []
search a s = regexec a s >>= \x -> 
			case x of
				Left err -> return []
				Right m -> case m of
					Nothing -> return []
					Just (_,match,after,_) -> do
						f <- search a after
						return $ match : f 