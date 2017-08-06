{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Bio.Data.Experiment.File where

import           Control.Lens        (makeFields)
import           Data.Aeson (ToJSON(..), Value(..), FromJSON(..), withArray)
import           Data.Aeson.TH       (defaultOptions, deriveJSON)
import           Data.Map.Strict     (Map)
import           Data.Serialize      (Serialize (..))
import           Data.Serialize.Text ()
import qualified Data.Text           as T
import           Data.Type.Bool
import           GHC.Generics        (Generic)
import qualified Data.Vector as V
import           GHC.TypeLits
import Data.Promotion.Prelude.Eq (PEq(..))
import Data.Singletons.TH

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
    deriving (Show, Read, Eq)

$(singletons [d|
    data FileTag = Sorted
                 | Pairend
                 | Gzip
        deriving (Show, Read, Eq)
    |])

data File (filetags :: [FileTag]) (filetype :: FileType) where
    File :: { fileLocation :: FilePath
            , fileInfo     :: (Map T.Text T.Text)
            , fileTags     :: [T.Text]
            } -> File filetags filetype
            deriving (Show, Read, Eq, Ord, Generic)

makeFields ''File
deriveJSON defaultOptions ''File
instance Serialize (File filetags filetype)

data FileList :: [[FileTag]] -> [FileType] -> * where
    FNil :: FileList '[] '[]
    FCons :: File tag filetype
          -> FileList tags filetypes
          -> FileList (tag ': tags) (filetype ': filetypes)

deriving instance Eq (FileList tags filetypes)
deriving instance Ord (FileList tags filetypes)

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
          go :: FileList tags filetypes -> [Value]
          go FNil = []
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
