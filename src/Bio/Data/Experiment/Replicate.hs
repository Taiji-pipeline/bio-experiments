{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
module Bio.Data.Experiment.Replicate where

import           Lens.Micro.TH (makeFields)
import           Lens.Micro (Lens)
import           Data.Aeson.TH            (defaultOptions, deriveJSON)
import           Data.Map.Strict          (Map)
import           Data.Binary (Binary(..))
import qualified Data.Text                as T
import           GHC.Generics             (Generic)

import Bio.Data.Experiment.File

data Replicate file = Replicate
    { replicateFiles   :: file
    , _replicateInfo   :: (Map T.Text T.Text)
    }
    deriving (Show, Generic)

makeFields ''Replicate

files :: Lens (Replicate file1) (Replicate file2) file1 file2
files f s = (\x -> s{replicateFiles = x}) <$> f (replicateFiles s)
{-# INLINE files #-}

deriveJSON defaultOptions ''Replicate
instance Binary file => Binary (Replicate file)
