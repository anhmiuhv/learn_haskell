
{-# LANGUAGE OverloadedStrings,RecordWildCards,ScopedTypeVariables #-}


module Main where


import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Lazy.Char8 as CL
import qualified Data.Vector as V 
import Data.Text    as T
import qualified Data.Text.IO as T
import Network.Http.Client
import Data.Char
import Control.Exception
import Helper
import Data.Aeson
import OpenSSL (withOpenSSL)


data Pronunciation = Pronunciation {
    word :: Text,
    pronun :: Text
}

instance FromJSON Pronunciation where
    parseJSON = withObject "pronun" $ \o -> do
        results <- V.head <$> o .: "results"
        word <- results .: "id"
        flex <- V.head <$> results .: "lexicalEntries"
        pronuns <- V.head <$> flex .: "pronunciations"
        pronun <- pronuns .: "phoneticSpelling"
        return Pronunciation {..}




app_id = C.pack "34993c48"
apli_id = C.pack "fce5d677e990d2a35dbb4ec960786fa9"
url = C.pack "od-api.oxforddictionaries.com"
main :: IO ()
main = main1 []

extract :: IO String
extract = do
            x <- getLine
            if x /= "" then return $ Prelude.filter isAlphaNum $ (Prelude.words x) !! 0
                       else extract

readInt::IO Int
readInt = readLn

getPos :: Int -> IO Int
getPos i = do 
    pos <- catch Main.readInt (\(e :: SomeException) -> retry)
    if pos < 1 || pos > i
        then retry
        else return pos
     where retry = do
            Prelude.putStrLn "invalid.Please reenter"
            getPos i

showText::[Text] -> Text
showText [] = ""
showText (x:xs) = x `T.append` "   " `T.append` showText xs


main1 :: [Pronunciation] -> IO () 
main1 x
    | Prelude.length x < 3 = withOpenSSL $ do
                        Prelude.putStrLn "Type in a word"
                        word1 <- extract
                        Prelude.putStrLn word1
                        ctx <- baselineContextSSL
                        c <- openConnectionSSL ctx url 443
                        let q = buildRequest1 $ do
                                    http GET $ (C.pack $ "/api/v1/entries/en/" ++ word1)
                                    setAccept (C.pack "text/html")
                                    setHeader (C.pack "app_id") app_id
                                    setHeader (C.pack "app_key") apli_id

                        sendRequest c q emptyBody
                        response <- receiveResponse c concatHandler 
                        
                        case decode $ CL.fromStrict response :: Maybe Pronunciation of
                            Just d -> do
                                T.putStrLn $ pronun d
                                xs <- return $ d:x
                                closeConnection c
                                main1 xs        
                            Nothing -> do
                                T.putStrLn " fuck you 1"
                                closeConnection c                               
                                main1 x
    | otherwise = do
        let p1 = hyphenated . pronun <$> x
        Prelude.putStrLn "Inspected words are:"
        Prelude.putStrLn $ show $ word <$> x
        Prelude.putStrLn "Their pronunciations:"
        T.putStrLn $ showText $ pronun <$> x
        Prelude.putStrLn "Type in the position of the inspected syllable in the first word"
        pos1 <- getPos $ (Prelude.length . Prelude.head) p1
        Prelude.putStrLn "Type in the position of the inspected syllable in the second word"
        pos2 <- getPos $ (Prelude.length . (!! 1)) p1
        Prelude.putStrLn "Type in the position of the inspected syllable in the third word"
        pos3 <- getPos $ (Prelude.length . (!! 2)) p1
        Prelude.putStrLn "The answer is:"
        determine (pos1, pos2, pos3) p1
        main1 []
