
module Main where

import Text.Regex.PCRE.String
import Text.Regex.PCRE
import Data.Maybe
main :: IO ()
main = do
	putStrLn "Type the name of the ocr files"
	filename <- getLine
	s <- readFile filename
	cs <- search s

	print cs
	
patt = makeRegexOpts compCaseless execBlank "(total fat|trans fat|sat fat|Sugars|proteins|carb)" :: Regex


search :: String -> IO [String]
search "" = return []
search s = regexec patt s >>= \x -> 
			case x of
				Left err -> return []
				Right m -> case m of
					Nothing -> return []
					Just (_,match,after,_) -> do
						f <- search after
						return $ match : f 