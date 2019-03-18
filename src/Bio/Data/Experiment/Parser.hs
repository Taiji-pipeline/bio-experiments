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

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TypeOperators         #-}

module Bio.Data.Experiment.Parser
    ( readATACSeq
    , readATACSeqTSV
    , readChIPSeq
    , readChIPSeqTSV
    , readRNASeq
    , readRNASeqTSV
    , readHiC
    , readHiCTSV
    , parseRNASeq
    , guessFormat
    ) where

import           Control.Arrow                 (first)
import           Data.Aeson
import           Data.Aeson.Internal           (JSONPathElement (..), (<?>))
import           Data.Aeson.Types
import           Data.CaseInsensitive          (CI, mk)
import qualified Data.HashMap.Strict           as HM
import qualified Data.IntMap.Strict            as IM
import           Data.List                     (nub)
import qualified Data.Map.Strict               as M
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Vector                   as V
import           Data.Yaml

import           Bio.Data.Experiment
import           Bio.Data.Experiment.File
import           Bio.Data.Experiment.Replicate
import           Bio.Data.Experiment.Types

type MaybePairSomeFile = Either SomeFile (SomeFile, SomeFile)

parseFile :: Value -> Parser SomeFile
parseFile = withObject "File" $ \obj' -> do
    let obj = toLowerKey obj'
    path <- obj .: "path"
    format <- obj .:? "format" .!= guessFormat path
    tags <- fmap (\x -> nub $ if gzipped path then Gzip : x else x) $
        obj .:? "tags" .!= []
    fl <- File <$> return path <*> obj .:? "info" .!= M.empty
    return $ addTags tags $ setFiletype format $ SomeFile (fl :: File '[] 'Other)

parseFilePair :: Value -> Parser MaybePairSomeFile
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

parseReplicate :: Value
               -> Parser (S (Replicate [MaybePairSomeFile]))
parseReplicate = withObject "Replicate" $ \obj' -> do
    let obj = toLowerKey obj'
    repNum <- obj .:? "rep" .!= 0
    rep <- Replicate <$> withParser (parseList parseFilePair) obj "files" <*>
        obj .:? "info" .!= M.empty
    return (repNum, rep)

parseCommonFields :: Value
                  -> Parser (CommonFields N [MaybePairSomeFile])
parseCommonFields = withObject "CommonFields" $ \obj' -> do
    let obj = toLowerKey obj'
    CommonFields <$> obj .: "id" <*>
                     obj .:? "group" <*>
                     obj .:? "celltype" .!= "" <*>
                     (IM.fromListWith errMsg <$>
                        withParser (parseList parseReplicate) obj "replicates")
  where
    errMsg = error "Different replicates should have unique replicate numbers"

                     {-
parseChIPSeq :: Value -> Parser (ChIPSeq [SomeFile])
parseChIPSeq = withObject "ChIPSeq" $ \obj' -> do
    let obj = toLowerKey obj'
    ChIPSeq <$> parseCommonFields (Object obj') <*>
                obj .: "target" <*>
                obj .:? "pairedend" .!= False <*>
                obj .:? "control"
                -}

readTSV :: FilePath -> IO [HM.HashMap T.Text T.Text]
readTSV input = do
    c <- T.readFile input
    let header : content = T.lines c
        fields = T.splitOn "\t" header
    return $ flip map content $ \l ->
        HM.fromList $ zip fields $ T.splitOn "\t" l


--------------------------------------------------------------------------------
-- ATACSeq
--------------------------------------------------------------------------------

readATACSeq :: FilePath -> T.Text
            -> IO [ATACSeq N [MaybePairSomeFile]]
readATACSeq input key = readFromFile input key parseATACSeq

parseATACSeq :: Value -> Parser (ATACSeq N [MaybePairSomeFile])
parseATACSeq = withObject "ATACSeq" $ \obj' -> do
    let obj = toLowerKey obj'
    ATACSeq <$> parseCommonFields (Object obj)

readATACSeqTSV :: FilePath -> T.Text -> IO [ATACSeq N [MaybePairSomeFile]]
readATACSeqTSV input key = do
    tsv <- readTSV input
    return $ mergeExp $ map (ATACSeq . mapToCommonFields) $
        filter ((== mk key) . mk . HM.lookupDefault "" "type") tsv


--------------------------------------------------------------------------------
-- ChIPSeq
--------------------------------------------------------------------------------

readChIPSeq :: FilePath -> T.Text
            -> IO [ChIPSeq N [MaybePairSomeFile]]
readChIPSeq input key = readFromFile input key parseChIPSeq

parseChIPSeq :: Value -> Parser (ChIPSeq N [MaybePairSomeFile])
parseChIPSeq = withObject "ChIPSeq" $ \obj' -> do
    let obj = toLowerKey obj'
    ChIPSeq <$> parseCommonFields (Object obj)

readChIPSeqTSV :: FilePath -> T.Text -> IO [ChIPSeq N [MaybePairSomeFile]]
readChIPSeqTSV input key = do
    tsv <- readTSV input
    return $ mergeExp $ map (ChIPSeq . mapToCommonFields) $
        filter ((== mk key) . mk . HM.lookupDefault "" "type") tsv


--------------------------------------------------------------------------------
-- RNASeq
--------------------------------------------------------------------------------

readRNASeq :: FilePath -> T.Text
            -> IO [RNASeq N [MaybePairSomeFile]]
readRNASeq input key = readFromFile input key parseRNASeq

readRNASeqTSV :: FilePath -> T.Text -> IO [RNASeq N [MaybePairSomeFile]]
readRNASeqTSV input key = do
    tsv <- readTSV input
    return $ mergeExp $ map (RNASeq . mapToCommonFields) $
        filter ((== mk key) . mk . HM.lookupDefault "" "type") tsv

parseRNASeq :: Value -> Parser (RNASeq N [MaybePairSomeFile])
parseRNASeq = withObject "RNASeq" $ \obj' -> do
    let obj = toLowerKey obj'
    RNASeq <$> parseCommonFields (Object obj)

--------------------------------------------------------------------------------
-- HiC
--------------------------------------------------------------------------------
readHiC :: FilePath -> T.Text
        -> IO [HiC N [MaybePairSomeFile]]
readHiC input key = readFromFile input key parseHiC

readHiCTSV :: FilePath -> T.Text -> IO [HiC N [MaybePairSomeFile]]
readHiCTSV input key = do
    tsv <- readTSV input
    return $ mergeExp $ map (HiC . mapToCommonFields) $
        filter ((== mk key) . mk . HM.lookupDefault "" "type") tsv

parseHiC :: Value -> Parser (HiC N [MaybePairSomeFile])
parseHiC = withObject "HiC" $ \obj' -> do
    let obj = toLowerKey obj'
    HiC <$> parseCommonFields (Object obj)


-- | Convert a dictionary to record
mapToCommonFields :: HM.HashMap T.Text T.Text -> CommonFields S MaybePairSomeFile
mapToCommonFields m = CommonFields
    { _commonEid = HM.lookupDefault (error "missing id!") "id" m
    , _commonGroupName = HM.lookup "group" m
    , _commonSampleName = HM.lookupDefault "" "celltype" m
    , _commonReplicates = (repNum, rep) }
  where
    repNum = read $ T.unpack $ HM.lookupDefault "0" "rep" m
    rep = Replicate
        { replicateFiles = fl
        , _replicateInfo = M.empty
        }
    filetype f = case HM.lookup "format" m of
        Nothing -> guessFormat f
        Just "" -> guessFormat f
        Just x  -> read $ T.unpack x
    tags f = checkGzipped f $ case HM.lookup "tags" m of
        Nothing -> []
        Just "" -> []
        Just x  -> map (read . T.unpack) $ T.splitOn "," x
      where
        checkGzipped x ts = nub $ if gzipped x then Gzip : ts else ts
    locations = map T.unpack $ T.splitOn "," $
        HM.lookupDefault (error "missing path") "path" m
    fl = case locations of
        [f] -> Left $ setFiletype (filetype f) $ addTags (tags f) $ SomeFile
            (File { fileLocation = f, fileInfo = M.empty } :: File '[] 'Other)
        [f1,f2] -> Right
            ( setFiletype (filetype f1) $ addTags (tags f1) $ SomeFile
                (File { fileLocation = f1, fileInfo = M.empty } :: File '[] 'Other)
            , setFiletype (filetype f2) $ addTags (tags f2) $ SomeFile
                (File { fileLocation = f2, fileInfo = M.empty } :: File '[] 'Other)
            )
        _ -> error "too many files"

readFromFile :: FilePath
             -> T.Text   -- ^ key
             -> (Value -> Parser a) -> IO [a]
readFromFile input key parser = do
    dat <- readYml input
    case HM.lookup (mk key) dat of
        Nothing -> return []
        Just x -> case parseEither (parseList parser) x of
            Left err -> error err
            Right r  -> return r
  where
    readYml :: FilePath -> IO (HM.HashMap (CI T.Text) Value)
    readYml fl = do
        result <- decodeFile fl
        case result of
            Nothing  -> error "Unable to read input file. Formatting error!"
            Just dat -> return $ HM.fromList $ map (first mk) $ HM.toList dat
{-# INLINE readFromFile #-}


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
