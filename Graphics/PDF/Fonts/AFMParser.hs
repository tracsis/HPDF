{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
---------------------------------------------------------
-- |
-- Copyright   : (c) 2006-2016, alpheccar.org
-- License     : BSD-style
--
-- Maintainer  : misc@NOSPAMalpheccar.org
-- Stability   : experimental
-- Portability : portable
--
-- AFM AFMParser
---------------------------------------------------------
module Graphics.PDF.Fonts.AFMParser(
      getFont
    , AFMFont(..)
    , EncodingScheme(..)
    , Metric(..)
    , KX(..)
    , parseFont
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BChar8
import           Control.Monad
import           Data.Char                     (toUpper)
import           Data.FileEmbed
import qualified Data.Map.Strict               as M
import           Data.Maybe                    (fromJust)
import           Graphics.PDF.Fonts.Encoding   (PostscriptName)
import           Graphics.PDF.Fonts.Font       (emptyFontStructure)
import           Graphics.PDF.Fonts.FontTypes
import           Graphics.PDF.LowLevel.Types
import           Text.Parsec                   (modifyState)
import           Text.Parsec.Prim              (parserZero)
import           Text.ParserCombinators.Parsec hiding (space)

data Metric = Metric { charCode    :: Int
                     , metricWidth :: Int
                     , name        :: String
                     , bounds      :: [Double]
                     }
                     deriving(Eq,Show)

data EncodingScheme = AFMAdobeStandardEncoding
                    | AFMFontSpecific
                    | AFMUnsupportedEncoding
                    deriving(Eq,Read,Show)

data KX = KX String String Int
        deriving(Eq,Ord,Show)

data AFMFont = AFMFont { metrics            :: [Metric]
                       , underlinePosition  :: Int
                       , underlineThickness :: Int
                       , afmAscent          :: Int
                       , afmDescent         :: Int
                       , kernData           :: Maybe [KX]
                       , type1BaseFont      :: String
                       , encodingScheme     :: EncodingScheme
                       , afmItalic          :: Double
                       , afmCapHeight       :: Int
                       , afmBBox            :: [Double]
                       , afmFixedPitch      :: Bool
                       , afmSymbolic        :: Bool
                       }
                       deriving(Eq,Show)


type AFMParser = GenParser Char AFMFont

emptyAFM :: AFMFont
emptyAFM = AFMFont { metrics = []
                   , underlinePosition = 0
                   , underlineThickness = 0
                   , afmAscent = 0
                   , afmDescent = 0
                   , kernData = Nothing
                   , type1BaseFont = ""
                   , encodingScheme = AFMAdobeStandardEncoding
                   , afmItalic = 0.0
                   , afmCapHeight = 0
                   , afmBBox = []
                   , afmFixedPitch = False
                   , afmSymbolic = False
                   }

capitalize :: String -> String
capitalize []    = []
capitalize (h:t) = toUpper h : t


line :: AFMParser ()
line = void $ string "\r\n" <|> string "\n"

toEndOfLine :: AFMParser ()
toEndOfLine = do _ <- many (noneOf "\r\n")
                 line
                 return ()

getString :: AFMParser String
getString = do
  c <- many1 (alphaNum <|> oneOf "-+")
  line
  return c

getInt :: AFMParser Int
getInt  = do c <- getString
             return $ read c

getFloat :: AFMParser Double
getFloat = do
                c <- many1 (alphaNum <|> oneOf ".-+")
                line
                return $ read c

getBool :: AFMParser Bool
getBool = do c <- getString
             return $ read (capitalize c)

data CharacterSet = ExtendedRoman
                  | Special
                  deriving(Eq,Read,Show)

data Weight = Medium
            | Bold
            | Roman
            deriving(Eq,Read,Show)


array :: AFMParser [String]
array = sepEndBy (many1 (oneOf "-+0123456789")) (many1 (oneOf " "))

getArray :: AFMParser [Double]
getArray  = do c <- array
               line
               return . map read $ c



getEncoding :: AFMParser EncodingScheme
getEncoding = do
  c <- getString
  case c of
    "AdobeStandardEncoding" -> return AFMAdobeStandardEncoding
    "FontSpecific"          -> return AFMFontSpecific
    _                       -> return  AFMUnsupportedEncoding

number :: AFMParser Int
number  = do c <- many1 (oneOf "-+0123456789")
             return $ read c

data Elem = C Int
          | WX Int
          | N String
          | B [Double]
          | L
          deriving(Eq,Read,Show)

metricElem :: AFMParser Elem
metricElem  = do _ <- char 'C'
                 spaces
                 c <- number
                 return $ C c
              <|>
              do _ <- string "WX"
                 spaces
                 c <- number
                 return $ WX c
              <|>
              do _ <- char 'N'
                 spaces
                 c <- many1 (alphaNum <|> char '.')
                 return $ N c
              <|>
              do _ <- char 'B'
                 spaces
                 c <- array
                 return . B . map read $ c
              <|>
              do _ <- char 'L'
                 spaces
                 _ <- many1 letter
                 spaces
                 _ <- many1 letter
                 return L

mkMetric :: [Elem] -> Metric
mkMetric l = foldr addElem (Metric (-1) 0 "" []) l
 where
     addElem  (C c) m  = m {charCode=c}
     addElem  (WX c) m = m {metricWidth=c}
     addElem  (N s) m  = m {name=s}
     addElem  (B b) m  = m {bounds=b}
     addElem  _ m      = m

charMetric :: AFMParser Metric
charMetric = do
       l <- sepEndBy metricElem (many1 (oneOf "; "))
       line
       return . mkMetric $ l



kernPair :: AFMParser KX
kernPair = do _ <- string "KPX"
              spaces
              namea <- many1 alphaNum
              spaces
              nameb <- many1 alphaNum
              spaces
              nb <- many1 (oneOf "-+0123456789")
              line
              return $ KX namea nameb (read nb)



keyword :: String -> AFMParser () -> AFMParser ()
keyword s action = do
  _ <- string s
  spaces
  action
  return ()

header :: String -> AFMParser ()
header s = do
  _ <- string s
  toEndOfLine
  return ()

notHeader :: String -> AFMParser ()
notHeader s = do
  r <- many1 alphaNum
  if s == r
    then
      parserZero
    else do
      toEndOfLine

specific :: AFMParser ()
specific = choice [ try $ keyword "FontName" (getString >>= \val -> modifyState $ \st -> st {type1BaseFont = val})
                  , try $ keyword "UnderlinePosition" (getInt >>= \val -> modifyState $ \st -> st {underlinePosition = val})
                  , try $ keyword "UnderlineThickness" (getInt >>= \val -> modifyState $ \st -> st {underlineThickness = val})
                  , try $ keyword "EncodingScheme" (getEncoding >>= \val -> modifyState $ \st -> st {encodingScheme = val})
                  , try $ keyword "CapHeight" (getInt >>= \val -> modifyState $ \st -> st {afmCapHeight = val})
                  , try $ keyword "Ascender" (getInt >>= \val -> modifyState $ \st -> st {afmAscent = val})
                  , try $ keyword "Descender" (getInt >>= \val -> modifyState $ \st -> st {afmDescent = val})
                  , try $ keyword "ItalicAngle" (getFloat >>= \val -> modifyState $ \st -> st {afmItalic = val})
                  , try $ keyword "IsFixedPitch" (getBool >>= \val -> modifyState $ \st -> st {afmFixedPitch = val})
                  , try $ keyword "FontBBox" (getArray >>= \val -> modifyState $ \st -> st {afmBBox = val})
                  , try $ notHeader "StartCharMetrics"
                  ]

getKernData :: AFMParser (Maybe [KX])
getKernData = do
            { header "StartKernData"
            ; header "StartKernPairs"
            ; k <- many1 kernPair
            ; header "EndKernPairs"
            ; header "EndKernData"
            ; return $ Just k
            }

afm :: AFMParser AFMFont
afm =
  do
    header "StartFontMetrics"
    _ <- many1 specific
    header "StartCharMetrics"
    charMetrics <- many1 charMetric
    header "EndCharMetrics"
    kerns <- option Nothing getKernData
    _ <- string "EndFontMetrics"

    modifyState $ \a -> a { metrics = charMetrics
                          , kernData = kerns
                          }

    a <- getState
    let [_,ymin,_,ymax] = afmBBox a
    if afmAscent a == 0
    then
       if afmCapHeight a /= 0
          then
              return $ a { afmAscent = afmCapHeight a
                         }
          else
              let h = floor (ymax - ymin) in
              return $ a { afmAscent = h
                         , afmDescent = 0
                         }
    else
       return $ a

addMetric :: M.Map PostscriptName GlyphCode -> Metric -> FontStructure -> FontStructure
addMetric nameToGlyph m fs =
    let c = M.lookup (name m) nameToGlyph
        fs' = case c of
                Just glyphCode ->
                  fs { widthData = M.insert (fromIntegral glyphCode) (fromIntegral $ metricWidth m) (widthData fs)}
                Nothing -> fs
    in
    case (name m) of
      "space"  -> fs' {space = fromIntegral $ charCode m}
      "hyphen" -> fs' {hyphen = Just (fromIntegral $ charCode m)}
      _        -> fs'

addKern :: M.Map String GlyphCode -> KX -> FontStructure -> FontStructure
addKern d (KX sa sb c) fs =
  let caM = M.lookup sa d
      cbM = M.lookup sb d
  in
  case (caM,cbM) of
    (Just ca, Just cb) -> fs {kernMetrics = M.insert (GlyphPair ca cb) (fromIntegral c) (kernMetrics fs)}
    _ -> fs

-- If the maybe argument is not nothing, we use the specific encoding for
-- the postscript names.
-- Otherwise we use the encoding we found in the afm file.
-- It is used to force MacRomanEncoding on not symbolic default fonts.
fontToStructure :: AFMFont
                -> M.Map PostscriptName Char
                -> Maybe (M.Map PostscriptName GlyphCode)
                -> FontStructure
fontToStructure a enc maybeMapNameToGlyph =
  let h = (afmAscent a - afmDescent a)
      fs = emptyFontStructure { descent = fromIntegral $ - (afmDescent a)
                              , height = fromIntegral $ h
                              , ascent = fromIntegral $ afmAscent a
                              , fontBBox = afmBBox a
                              , italicAngle = afmItalic a
                              , capHeight = fromIntegral $ afmCapHeight a
                              , fixedPitch = afmFixedPitch a
                              , serif = False
                              , symbolic = afmSymbolic a
                              , script = False
                              , nonSymbolic = not (afmSymbolic a)
                              , italic = False
                              , allCap = False
                              , smallCap = False
                              , forceBold = False
                              , baseFont = type1BaseFont a
                              }
      addName m d | charCode m == -1 = d
                  | otherwise = M.insert (name m) (fromIntegral $ charCode m) d
      nameToGlyph = maybe (foldr addName M.empty (metrics a)) id maybeMapNameToGlyph
      fs1 = foldr (addMetric nameToGlyph) fs (metrics a)
      addEncodingMapping (pname,glyphcode) d =
         let unicodeM = M.lookup pname enc
         in
         case unicodeM of
          Nothing   -> d
          Just code -> M.insert code glyphcode d
      mapping = foldr addEncodingMapping M.empty (M.toList nameToGlyph)
      fs2 = fs1 { encoding = mapping}
  in
  case kernData a of
    Nothing -> fs2
    Just k  -> foldr (addKern nameToGlyph) fs2 k

afmParseFromFile :: AFMParser AFMFont -> FilePath -> IO (Either ParseError AFMFont)
afmParseFromFile p path = do
  l <- readFile path
  return $ runParser p emptyAFM path l

afmParseFromFileBytes :: AFMParser AFMFont -> FilePath -> B.ByteString -> IO (Either ParseError AFMFont)
afmParseFromFileBytes p path bytes = return $ runParser p emptyAFM path (BChar8.unpack bytes)

core14_AFMsDir :: M.Map FilePath B.ByteString
core14_AFMsDir = M.fromList $(embedDir "Core14_AFMs")

parseFont :: Either String String -> IO (Maybe AFMFont)
parseFont (Left core14_file) = do
    let bytes = fromJust $ M.lookup core14_file core14_AFMsDir
    r <- afmParseFromFileBytes afm core14_file bytes
    case r of
      Left e  -> error (show e)
      Right f -> return $ Just f
parseFont (Right path) = do
    r <- afmParseFromFile afm path
    case r of
      Left e  -> error (show e)
      Right f -> return $ Just f

getFont :: Either String AFMFont
        -> M.Map PostscriptName Char  -- ^ Glyph name to unicode
        -> Maybe (M.Map PostscriptName GlyphCode)  -- ^ Glyph name to glyph code if not standard coding
        -> IO (Maybe FontStructure)
getFont (Left s) enc nameToGlyph = do
  result <- parseFont (Left s)
  case result of
    Nothing -> return Nothing
    Just r  -> return (Just $ fontToStructure r enc nameToGlyph)
getFont (Right result) enc nameToGlyph = return . Just $ fontToStructure result enc nameToGlyph

