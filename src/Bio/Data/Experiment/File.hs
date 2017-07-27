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

import           Control.Lens        (makeFields, makePrisms)
import           Data.Aeson.TH       (defaultOptions, deriveJSON)
import           Data.HVect          (HVect (..))
import           Data.Map.Strict     (Map)
import           Data.Serialize      (Serialize (..))
import           Data.Serialize.Text ()
import qualified Data.Text           as T
import           Data.Type.Bool
import           GHC.Generics        (Generic)
import           GHC.TypeLits

data FileType = BamFile
              | BaiFile
              | BedFile
              | FastqFile
              | BedgraphFile
              | BigWigFile
              | NarrowPeakFile
              | BroadPeakFile
              | SRA
              | Other
    deriving (Show, Read, Eq, Ord)

data File (filetype :: FileType) where
    File :: { fileLocation :: FilePath
            , fileInfo     :: (Map T.Text T.Text)
            , fileTags     :: [T.Text]
            , fileGzipped  :: Bool
            } -> File filetype
            deriving (Show, Read, Eq, Ord, Generic)

makeFields ''File
deriveJSON defaultOptions ''File
instance Serialize (File filetype)

data FileSet f1 f2 = Single (File f1)
                   | Paired (File f1) (File f2)
            deriving (Show, Read, Eq, Ord, Generic)

makePrisms ''FileSet
deriveJSON defaultOptions ''FileSet
instance (Serialize (File f1), Serialize (File f2)) => Serialize (FileSet f1 f2)

type family BioData filelist :: Bool where
    BioData (File f) = 'True
    BioData [File f] = 'True
    BioData (HVect '[]) = 'True
    BioData (HVect (f ': fs)) = BioData f && BioData (HVect fs)
    BioData a = TypeError ( Text "type ‘" :<>:
        ShowType a :<>: Text "’ is not an instance of 'BioData'.")
