{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
module Bio.Data.Experiment.File where

import           Control.Lens        (makeFields)
import           Data.Aeson.TH       (defaultOptions, deriveJSON)
import           Data.HVect          (HVect (..))
import           Data.Map.Strict     (Map)
import           Data.Serialize      (Serialize (..))
import           Data.Serialize.Text ()
import qualified Data.Text           as T
import           Data.Type.Bool
import           GHC.Generics        (Generic)
import           GHC.TypeLits

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
    deriving (Show, Read, Eq, Ord)

data FileTag = Sorted
             | Pairend
             | GZipped

data File (filetags :: [FileTag]) (filetype :: FileType) where
    File :: { fileLocation :: FilePath
            , fileInfo     :: (Map T.Text T.Text)
            , fileTags     :: [T.Text]
            } -> File filetags filetype
            deriving (Show, Read, Eq, Ord, Generic)

makeFields ''File
deriveJSON defaultOptions ''File
instance Serialize (File filetags filetype)

type family BioData filetype :: Bool where
    BioData (f1, f2) = BioData f1 && BioData f2
    BioData (File tags f) = 'True
    BioData [f] = BioData f
    BioData (Either f1 f2) = BioData f1 && BioData f2
    BioData (HVect '[]) = 'True
    BioData (HVect (f ': fs)) = BioData f && BioData (HVect fs)
    BioData a = TypeError ( 'Text "type ‘" ':<>:
        'ShowType a ':<>: 'Text "’ is not an instance of 'BioData'.")
