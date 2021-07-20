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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TypeOperators         #-}

module Bio.Data.Experiment.Parser
    ( mkInputReader
    , guessFormat
    ) where

import           Control.Arrow                 (first)
import Control.Exception
import           Data.Aeson
import           Data.Aeson.Types
import           Data.CaseInsensitive          (CI, mk)
import qualified Data.HashMap.Strict           as HM
import qualified Data.IntMap.Strict            as IM
import           Data.List                     (nub, groupBy, sortBy)
import qualified Data.Map.Strict               as M
import Data.Function (on)
import Data.Ord (comparing)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Vector                   as V
import           Data.Yaml
import qualified Dhall.Yaml as D
import Control.Monad (unless)
import System.IO

import           Bio.Data.Experiment
import           Bio.Data.Experiment.File
import           Bio.Data.Experiment.Types

mkInputReader :: Experiment e
              => FilePath  -- ^ Input file
              -> T.Text    -- key
              -> (CommonFields N [MaybePairSomeFile] -> e N [MaybePairSomeFile])
              -> IO [e N [MaybePairSomeFile]]
mkInputReader input key constructor
    | suffix == "dhall" = dhallReader input
    | suffix == "yml" || suffix == "yaml" = ymlReader input
    | suffix == "tsv" = tsvReader input
    | otherwise = try (ymlReader input) >>= \case
        Right res -> return res
        Left (SomeException e) -> do
            putStrLn "Parsing input file as YAML format failed because:"
            print e
            putStrLn "Now trying TSV format..."
            tsvReader input
  where
    tsvReader i = do
        tsv <- readTSV i
        validate $ concatMap (mergeExp . map mapToCommonFields) $
            groupBy ((==) `on` getType) $ sortBy (comparing getType) tsv
        return $ map constructor $ mergeExp $ map mapToCommonFields $
            filter ((== mk key) . mk . HM.lookupDefault "" "type") tsv
      where
        getType x = mk $ HM.lookupDefault "" "type" x
    ymlReader i = do
        dat <- fmap (either error id . parseEither (parseList parser)) <$>
            readYml i
        validate $ concat $ HM.elems dat
        case HM.lookup (mk key) dat of
            Nothing -> return []
            Just x -> return $ map constructor x
      where
        parser = withObject "E" $ \obj' ->
            parseCommonFields (Object $ toLowerKey obj')
    dhallReader i = do
        let output = i <> ".yaml"
        T.readFile i >>= D.dhallToYaml D.defaultOptions Nothing >>= B.writeFile output
        ymlReader output
    suffix = snd $ T.breakOnEnd "." $ T.pack input

validate :: [CommonFields N [MaybePairSomeFile]] -> IO ()
validate input = unless (null redudant) $ do
    error $ "Multiple IDs exist for: " <> show redudant
  where
    ids = map _commonEid input
    redudant = HM.keys $ HM.filter (>1) $ HM.fromListWith (+) $ zip ids $ repeat (1 :: Int)
{-# INLINE validate #-}

readTSV :: FilePath -> IO [HM.HashMap T.Text T.Text]
readTSV input = withFile input ReadMode $ \h -> do
    hSetNewlineMode h universalNewlineMode
    (header : content) <- map (T.splitOn "\t") . T.lines <$> T.hGetContents h
    return $ map (HM.fromList . zip header) content
{-# INLINE readTSV #-}

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

readYml :: FilePath -> IO (HM.HashMap (CI T.Text) Value)
readYml fl = decodeFileEither fl >>= \case
    Left x -> error $ "Unable to read input file: " <> show x
    Right dat -> return $ HM.fromList $ map (first mk) $ HM.toList dat
{-# INLINE readYml #-}

--------------------------------------------------------------------------------
-- Parsers
--------------------------------------------------------------------------------

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
{-# INLINE parseFile #-}

parseFilePair :: Value -> Parser MaybePairSomeFile
parseFilePair = withObject "FileSet" $ \obj' -> do
    let obj = toLowerKey obj'
    fls <- obj .:? "pair"
    case fls of
        Nothing -> Left <$> parseFile (Object obj')
        Just a -> flip (withArray "FileSet") a $ \xs ->
            if V.length xs == 2
                then fmap Right $ (,) <$> parseFile (xs `V.unsafeIndex` 0)
                         <*> parseFile (xs `V.unsafeIndex` 1)
                else error "The number of files must be 2."
{-# INLINE parseFilePair #-}

guessFormat :: FilePath -> FileType
guessFormat fl = case () of
    _ | ".bam" `T.isSuffixOf` fl' -> Bam
      | ".bai" `T.isSuffixOf` fl' -> Bai
      | ".bed" `T.isSuffixOf` fl' -> Bed
      | ".bed.gz" `T.isSuffixOf` fl' -> Bed
      | ".fastq" `T.isSuffixOf` fl' -> Fastq
      | ".fq" `T.isSuffixOf` fl' -> Fastq
      | ".fastq.gz" `T.isSuffixOf` fl' -> Fastq
      | ".fq.gz" `T.isSuffixOf` fl' -> Fastq
      | ".bw" `T.isSuffixOf` fl' -> BigWig
      | otherwise -> Other
  where
    fl' = T.pack fl
{-# INLINE guessFormat #-}

gzipped :: FilePath -> Bool
gzipped fl = ".gz" `T.isSuffixOf` T.pack fl
{-# INLINE gzipped #-}

parseReplicate :: Value
               -> Parser (S (Replicate [MaybePairSomeFile]))
parseReplicate = withObject "Replicate" $ \obj' -> do
    let obj = toLowerKey obj'
    repNum <- obj .:? "rep" .!= 0
    rep <- Replicate <$> withParser (parseList parseFilePair) obj "files" <*>
        obj .:? "info" .!= M.empty
    return (repNum, rep)
{-# INLINE parseReplicate #-}

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
{-# INLINE parseCommonFields #-}

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