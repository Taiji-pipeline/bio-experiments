{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE EmptyCase              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Bio.Data.Experiment.File where

import           Lens.Micro.TH (makeFields)
import           Data.Aeson.TH           (defaultOptions, deriveJSON)
import           Data.Coerce             (coerce)
import           Data.List               (foldl')
import qualified Data.Map.Strict         as M
import           Data.Binary (Binary(..))
import           Data.Singletons.Prelude
import           Data.Singletons.TH
import qualified Data.Text               as T
import           GHC.Generics            (Generic)

-- | Formats of files
$(singletons [d|
    data FileType = Bam
                  | Bai
                  | Bed
                  | Fastq
                  | Bedgraph
                  | BigWig
                  | BigBed
                  | NarrowPeak
                  | BroadPeak
                  | SRA
                  | Tsv
                  | MatrixMarket
                  | Other
        deriving (Show, Read, Eq, Ord, Generic)
    |])

deriveJSON defaultOptions ''FileType
instance Binary FileType


-- | Tags of files
$(singletons [d|
    data FileTag = CoordinateSorted
                 | NameSorted
                 | PairedEnd
                 | Gzip
                 | GeneQuant
                 | TranscriptQuant
                 | Filtered
                 | ENCODE
                 | ColumnName
                 | RowName
                 | ChromosomeLoop
                 | TXG    -- ^ 10X genomics
                 | Demultiplexed
        deriving (Show, Read, Eq, Ord, Generic)
    |])

deriveJSON defaultOptions ''FileTag
instance Binary FileTag


-- | File
data File (filetags :: [FileTag]) (filetype :: FileType) where
    File :: { fileLocation :: FilePath
            , fileInfo     :: (M.Map T.Text T.Text)
            } -> File filetags filetype
            deriving (Show, Read, Generic)

makeFields ''File
deriveJSON defaultOptions ''File
instance Binary (File filetags filetype)

-- | Opaque File
data SomeFile where
    SomeFile :: (SingI filetype, SingI filetags)
             => File filetags filetype -> SomeFile

instance Binary SomeFile where
    put fl = case fl of
        SomeFile (fl' :: File filetag filetype) -> do
            put $ fromSing (sing :: Sing filetag)
            put $ fromSing (sing :: Sing filetype)
            put fl'
    get = do
        filetags <- get :: _ [FileTag]
        filetype <- get :: _ FileType
        case toSing filetags of
            SomeSing (tag :: Sing tags) -> case toSing filetype of
                SomeSing (ft :: SFileType ft) -> withSingI tag $ withSingI ft $
                    SomeFile <$> (get :: _ (File tags ft))

instance Show SomeFile where
    show fl = case fl of
        SomeFile (fl' :: File filetag filetype) -> show
            ( fromSing (sing :: Sing filetag)
            , fromSing (sing :: Sing filetype)
            , fl' )

fromSomeFile :: SomeFile -> File tag filetype
fromSomeFile x = case x of
    SomeFile fl -> coerce fl

data SomeTags filetype where
    SomeTags :: SingI filetags
             => File filetags filetype -> SomeTags filetype

instance Binary (SomeTags filetype) where
    put fl = case fl of
        SomeTags (fl' :: File filetag filetype) -> do
            put $ fromSing (sing :: Sing filetag)
            put fl'
    get = do
        filetags <- get :: _ [FileTag]
        case toSing filetags of
            SomeSing (tag :: Sing tags) -> withSingI tag $
                SomeTags <$> (get :: _ (File tags filetype))

instance Show (SomeTags filetype) where
    show fl = case fl of
        SomeTags (fl' :: File filetag filetype) -> show
            ( fromSing (sing :: Sing filetag)
            , fl' )

fromSomeTags :: SomeTags filetype -> File tag filetype
fromSomeTags x = case x of
    SomeTags fl -> coerce fl
{-# INLINE fromSomeTags #-}

class FileTypeInfo f where
    getFileType :: f -> FileType

instance SingI filetype => FileTypeInfo (File tags filetype) where
    getFileType _ = fromSing (sing :: Sing filetype)

instance FileTypeInfo SomeFile where
    getFileType fl = case fl of
        SomeFile fl' -> getFileType fl'

instance SingI filetype => FileTypeInfo (SomeTags filetype) where
    getFileType _ = fromSing (sing :: Sing filetype)

class FileTagInfo f where
    getFileTags :: f -> [FileTag]
    hasTag :: f -> FileTag -> Bool
    hasTag x t = t `elem` getFileTags x

instance SingI tags => FileTagInfo (File tags filetype) where
    getFileTags _ = fromSing (sing :: Sing tags)

instance FileTagInfo SomeFile where
    getFileTags fl = case fl of
        SomeFile fl' -> getFileTags fl'

instance FileTagInfo (SomeTags filetype) where
    getFileTags fl = case fl of
        SomeTags fl' -> getFileTags fl'


castFile :: SomeFile -> SomeTags filetype
castFile fl = case fl of
    SomeFile (fl' :: File filetags _) ->
        SomeTags (coerce fl' :: File filetags filetype)
{-# INLINE castFile #-}

addTags :: [FileTag] -> SomeFile -> SomeFile
addTags ts file = foldl' f file ts
  where
    f fl tag = case fl of
        SomeFile fl' -> case toSing tag of
            SomeSing tag' -> withSingI tag' $ SomeFile $ addTag tag' fl'
    addTag :: SFileTag tag -> File tags ft -> File (tag ': tags) ft
    addTag _ fl = coerce fl
{-# INLINE addTags #-}

setFiletype :: FileType -> SomeFile -> SomeFile
setFiletype ft file = case file of
    SomeFile fl' -> case toSing ft of
        SomeSing ft' -> withSingI ft' $ SomeFile $ setFt ft' fl'
  where
    setFt :: SFileType ft' -> File tags ft -> File tags ft'
    setFt _ fl = coerce fl
{-# INLINE setFiletype #-}
