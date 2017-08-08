{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TypeOperators         #-}
-- |
-- Module:      Bio.Data.Experiment.Parser
-- Copyright:   (c) 2017 Kai Zhang
-- License:     BSD3
-- Maintainer:  Kai Zhang <kai@kzhang.org>
-- Stability:   experimental
-- Portability: portable
--
-- The default serialization of most types are too verbose and not suitable for
-- direct use by human beings. This module provids a more succint and
-- user-friendly way to write YAML files. Featuring:
-- 1. Most keywords have been renamed.
-- 2. Keywords are case-insensitive.
-- 3. Providing default values for some fields.

module Bio.Data.Experiment.Parser
    ( readATACSeq
    , parseRNASeq
    ) where

import           Control.Arrow                 (first)
import           Data.Aeson
import           Data.Aeson.Internal           (JSONPathElement (..), (<?>))
import           Data.Aeson.Types
import           Data.Coerce                   (coerce)
import qualified Data.HashMap.Strict           as HM
import           Data.List                     (foldl', nub)
import qualified Data.Map.Strict               as M
import           Data.Maybe                    (fromMaybe)
import           Data.Singletons
import qualified Data.Text                     as T
import qualified Data.Vector                   as V
import Data.Yaml
import Data.CaseInsensitive (CI, mk)

import           Bio.Data.Experiment.File
import           Bio.Data.Experiment.Replicate
import           Bio.Data.Experiment.Types
import           Bio.Data.Experiment.ATACSeq
import           Bio.Data.Experiment.RNASeq

addTag :: SFileTag tag -> File tags ft -> File (tag ': tags) ft
addTag _ fl = coerce fl

setFiletype :: SFileType ft' -> File tags ft -> File tags ft'
setFiletype _ fl = coerce fl

parseFile :: Value -> Parser SomeFile
parseFile = withObject "File" $ \obj' -> do
    let obj = toLowerKey obj'
    path <- obj .: "path"
    format <- obj .:? "format" .!= guessFormat path
    tags <- fmap (\x -> nub $ if gzipped path then Gzip : x else x) $
        obj .:? "tags" .!= []
    fl <- File <$> return path <*> obj .:? "info" .!= M.empty <*> return []
    return $ foldl' f (g (SomeFile (fl :: File '[] 'Other)) format) tags
  where
    f fl tag = case fl of
        SomeFile fl' -> case toSing tag of
            SomeSing tag' -> withSingI tag' $ SomeFile $ addTag tag' fl'
    g fl ft = case fl of
        SomeFile fl' -> case toSing ft of
            SomeSing ft' -> withSingI ft' $ SomeFile $ setFiletype ft' fl'

parseFilePair :: Value -> Parser (MaybePair SomeFile)
parseFilePair = withObject "FileSet" $ \obj' -> do
    let obj = toLowerKey obj'
    fls <- obj .:? "pair"
    case fls of
        Nothing -> Left <$> parseFile (Object obj')
        Just array -> flip (withArray "FileSet") array $ \xs ->
            if V.length xs == 2
                then fmap Right $ (,) <$> parseFile (xs `V.unsafeIndex` 0)
                         <*> parseFile (xs `V.unsafeIndex` 1)
                else error "The number of files must be 2."

guessFormat :: FilePath -> FileType
guessFormat fl = case () of
    _ | ".bam" `T.isSuffixOf` fl' -> Bam
      | ".bai" `T.isSuffixOf` fl' -> Bai
      | ".bed" `T.isSuffixOf` fl' -> Bed
      | ".bed.gz" `T.isSuffixOf` fl' -> Bed
      | ".fastq" `T.isSuffixOf` fl' -> Fastq
      | ".fastq.gz" `T.isSuffixOf` fl' -> Fastq
      | ".bw" `T.isSuffixOf` fl' -> BigWig
      | otherwise -> Other
  where
    fl' = T.pack fl

gzipped :: FilePath -> Bool
gzipped fl = ".gz" `T.isSuffixOf` T.pack fl

parseReplicate :: Value -> Parser (Replicate [MaybePair SomeFile])
parseReplicate = withObject "Replicate" $ \obj' -> do
    let obj = toLowerKey obj'
    Replicate <$> withParser (parseList parseFilePair) obj "files" <*>
                  obj .:? "info" .!= M.empty <*>
                  obj .:? "rep" .!= 0

parseCommonFields :: Value -> Parser (CommonFields [MaybePair SomeFile])
parseCommonFields = withObject "CommonFields" $ \obj' -> do
    let obj = toLowerKey obj'
    CommonFields <$> obj .: "id" <*>
                     obj .:? "group" <*>
                     obj .:? "celltype" .!= "" <*>
                     withParser (parseList parseReplicate) obj "replicates"

                     {-
parseChIPSeq :: Value -> Parser (ChIPSeq [SomeFile])
parseChIPSeq = withObject "ChIPSeq" $ \obj' -> do
    let obj = toLowerKey obj'
    ChIPSeq <$> parseCommonFields (Object obj') <*>
                obj .: "target" <*>
                obj .:? "pairedend" .!= False <*>
                obj .:? "control"
                -}

readATACSeq :: FilePath -> T.Text -> IO [ATACSeq [MaybePair SomeFile]]
readATACSeq input key = do
    dat <- readYml input
    return $ fromMaybe [] $ HM.lookup (mk key) dat >>= parseMaybe (parseList parseATACSeq)
  where
    readYml :: FilePath -> IO (HM.HashMap (CI T.Text) Value)
    readYml fl = do
        result <- decodeFile fl
        case result of
            Nothing  -> error "Unable to read input file. Formatting error!"
            Just dat -> return $ HM.fromList $ map (first mk) $ HM.toList dat

parseATACSeq :: Value -> Parser (ATACSeq [MaybePair SomeFile])
parseATACSeq = withObject "ATACSeq" $ \obj' -> do
    let obj = toLowerKey obj'
    ATACSeq <$> parseCommonFields (Object obj') <*>
                obj .:? "pairedend" .!= False

parseRNASeq :: Value -> Parser (RNASeq [MaybePair SomeFile])
parseRNASeq = withObject "RNASeq" $ \obj' -> do
    let obj = toLowerKey obj'
    RNASeq <$> parseCommonFields (Object obj') <*>
                obj .:? "pairedend" .!= False

                {-
parseHiC :: Value -> Parser HiC
parseHiC = withObject "HiC" $ \obj' -> do
    let obj = toLowerKey obj'
    HiC <$> parseCommonFields (Object obj')
    -}


--------------------------------------------------------------------------------

toLowerKey :: HM.HashMap T.Text a -> HM.HashMap T.Text a
toLowerKey = HM.fromList . map (first T.toLower) . HM.toList
{-# INLINE toLowerKey #-}

withParser :: (Value -> Parser a) -> Object -> T.Text -> Parser a
withParser p obj key = case HM.lookup key obj of
    Nothing -> fail $ "key " ++ show key ++ " not present"
    Just v  -> p v <?> Key key
{-# INLINE withParser #-}

parseList :: (Value -> Parser a) -> Value -> Parser [a]
parseList p (Array a) = mapM p $ V.toList a
parseList _ _         = error "Not a list"
{-# INLINE parseList #-}
