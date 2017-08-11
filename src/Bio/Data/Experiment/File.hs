{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE InstanceSigs #-}

module Bio.Data.Experiment.File where

import           Control.Lens              (makeFields)
import           Data.Aeson                (FromJSON (..), ToJSON (..),
                                            Value (..), withArray, (.=), object, withObject, (.:))
import           Data.Aeson.TH             (defaultOptions, deriveJSON)
import qualified Data.Map.Strict           as M
import           Data.Promotion.Prelude
import           Data.Singletons.Prelude
import           Data.Serialize            (Serialize (..))
import           Data.Serialize.Text       ()
import           Data.Singletons.TH
import qualified Data.Text                 as T
import           Data.Type.Bool
import qualified Data.Vector               as V
import           GHC.Generics              (Generic)
import           GHC.TypeLits
import           Data.Coerce                   (coerce)

-- | Formats of files
$(singletons [d|
    data FileType = Bam
                  | Bai
                  | Bed
                  | Fastq
                  | Bedgraph
                  | BigWig
                  | NarrowPeak
                  | BroadPeak
                  | SRA
                  | Tsv
                  | Other
        deriving (Show, Read, Eq, Ord, Generic)
    |])

deriveJSON defaultOptions ''FileType
instance Serialize FileType


-- | Tags of files
$(singletons [d|
    data FileTag = Sorted
                 | Pairend
                 | Gzip
        deriving (Show, Read, Eq, Ord, Generic)
    |])

deriveJSON defaultOptions ''FileTag
instance Serialize FileTag


-- | File
data File (filetags :: [FileTag]) (filetype :: FileType) where
    File :: { fileLocation :: FilePath
            , fileInfo     :: (M.Map T.Text T.Text)
            , fileTags     :: [FileTag]
            } -> File filetags filetype
            deriving (Show, Read, Generic)

makeFields ''File
deriveJSON defaultOptions ''File
instance Serialize (File filetags filetype)

getFileType :: forall tags (filetype :: FileType) . SingI filetype
            => File tags filetype -> FileType
getFileType _ = fromSing (sing :: Sing filetype)


-- | Opaque File
data SomeFile where
    SomeFile :: (SingI filetype, SingI filetags)
             => File filetags filetype -> SomeFile

instance Serialize SomeFile where
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

instance ToJSON SomeFile where
    toJSON fl = case fl of
        SomeFile (fl' :: File filetag filetype) -> object
            [ "filetag" .= fromSing (sing :: Sing filetag)
            , "filetype" .= fromSing (sing :: Sing filetype)
            , "data" .= fl' ]

instance FromJSON SomeFile where
    parseJSON = withObject "SomeFile" $ \obj -> do
        filetags <- obj .: "filetag" :: _ [FileTag]
        filetype <- obj .: "filetype" :: _ FileType
        case toSing filetags of
            SomeSing (tag :: Sing tags) -> case toSing filetype of
                SomeSing (ft :: SFileType ft) -> withSingI tag $ withSingI ft $
                    SomeFile <$> (obj .: "data" :: _ (File tags ft))

someFileIs :: SomeFile -> FileType -> Bool
someFileIs fl ft = case fl of
    SomeFile fl' -> getFileType fl' == ft

fromSomeFile :: SomeFile -> File tag filetype
fromSomeFile x = case x of
    SomeFile fl -> coerce fl

data SomeTags filetype where
    SomeTags :: SingI filetags
             => File filetags filetype -> SomeTags filetype

type FilePair tags filetype = (File tags filetype, File tags filetype)
type MaybePair tags1 tags2 filetype = Either (File tags1 filetype) (FilePair tags2 filetype)
type EitherTag tags1 tags2 filetype = Either (File tags1 filetype) (File tags2 filetype)

data FileList :: [[FileTag]] -> [FileType] -> * where
    FNil :: FileList '[] '[]
    FCons :: File tag filetype
          -> FileList tags filetypes
          -> FileList (tag ': tags) (filetype ': filetypes)

instance Serialize (FileList '[] '[]) where
    put _ = put (0 :: Int)
    get = do
        i <- get
        case (i :: Int) of
            0 -> return FNil
            _ -> error "decode fail"

instance Serialize (FileList tags filetypes) =>
    Serialize (FileList (tag ': tags) (filetype ': filetypes) ) where
        put (FCons x xs) = put x >> put xs
        get = FCons <$> get <*> get

instance ToJSON (FileList tags filetypes) where
    toJSON fs = Array $ V.fromList $ go fs
        where
          go :: FileList tags' filetypes' -> [Value]
          go FNil         = []
          go (FCons x xs) = toJSON x : go xs

instance FromJSON (FileList '[] '[]) where
    parseJSON = withArray "FileList" $ \xs -> if V.null xs
        then return FNil
        else error "Non-mepty List"

instance FromJSON (FileList tags filetypes) =>
    FromJSON (FileList (tag ': tags) (filetype ': filetypes)) where
        parseJSON = withArray "FileList" (go . V.toList)
          where
            go (x:xs) = FCons <$> parseJSON x <*> parseJSON (Array $ V.fromList xs)
            go _ = error "Not enough length"

type family BioData filetype :: Bool where
    BioData (f1, f2) = BioData f1 && BioData f2
    BioData (File tags f) = 'True
    BioData [f] = BioData f
    BioData (Either f1 f2) = BioData f1 && BioData f2
    BioData (FileList tags filetypes) = 'True
    BioData a = TypeError ( 'Text "type ‘" ':<>:
        'ShowType a ':<>: 'Text "’ is not an instance of 'BioData'.")
