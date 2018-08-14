{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE TemplateHaskell #-}
---------------------------------------------------------
-- |
-- Copyright   : (c) 2006-2016, alpheccar.org
-- License     : BSD-style
--
-- Maintainer  : misc@NOSPAMalpheccar.org
-- Stability   : experimental
-- Portability : portable
--
-- AFM Parser
---------------------------------------------------------
module Graphics.PDF.Fonts.Encoding(
      getEncoding
    , Encodings(..)
    , PostscriptName
    , parseMacEncoding
    ) where

import qualified Data.ByteString
import qualified Data.ByteString.Char8       as C
import           Data.Char                   (digitToInt)
import           Data.FileEmbed
import qualified Data.Map.Strict             as M
import           Data.Maybe                  (mapMaybe, fromJust)
import           Graphics.PDF.LowLevel.Types
import           System.FilePath

type PostscriptName = String

data Encodings = AdobeStandardEncoding
               | ZapfDingbatsEncoding
               deriving(Eq)

encodingsDir :: M.Map FilePath Data.ByteString.ByteString
encodingsDir = M.fromList $(embedDir "Encodings")

isLine :: C.ByteString -> Bool
isLine c | not (C.null c) = C.head c /= '#'
         | otherwise = False

from4Hexa :: C.ByteString -> Int
from4Hexa a = sum . map (uncurry (*)) $ zip (map digitToInt . C.unpack $ a)  (map (\x -> 16^x) ([3,2,1,0] :: [Int]))

from3Octal:: C.ByteString -> Int
from3Octal a = sum . map (uncurry (*)) $ zip (map digitToInt . C.unpack $ a)  (map (\x -> 8^x) ([2,1,0] :: [Int]))


toData :: [C.ByteString] -> Maybe (PostscriptName,Char)
toData (a:b:_) = Just (C.unpack a,toEnum . from4Hexa $ b)
toData _       = Nothing

toMacData :: [C.ByteString] -> Maybe (PostscriptName,GlyphCode)
toMacData (name:_:mac:_) | C.unpack mac == "-" = Nothing
                         | otherwise = Just (C.unpack name,fromIntegral (from3Octal mac))
toMacData _ = Nothing

parseGlyphListEncoding :: String -> IO (M.Map PostscriptName Char)
parseGlyphListEncoding name = do
  let bytes = fromJust $ M.lookup name encodingsDir
  return (M.fromList . mapMaybe (toData  . C.split ';') . filter isLine . C.lines $ bytes)

parseMacEncoding :: IO (M.Map PostscriptName GlyphCode)
parseMacEncoding = do
  let bytes = fromJust $ M.lookup "pdfencodings.txt" encodingsDir
  return . M.fromList . mapMaybe (toMacData . C.split '\t') . tail . C.lines $ bytes


getEncoding :: Encodings -> IO (M.Map PostscriptName Char)
getEncoding AdobeStandardEncoding = parseGlyphListEncoding $ "glyphlist" <.> "txt"
getEncoding ZapfDingbatsEncoding= parseGlyphListEncoding $ "zapfdingbats" <.> "txt"
