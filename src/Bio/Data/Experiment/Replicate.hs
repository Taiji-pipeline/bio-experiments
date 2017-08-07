{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}
module Bio.Data.Experiment.Replicate where

import Data.Map.Strict (Map)
import qualified Data.Text as T
import           GHC.Generics        (Generic)
import           Control.Lens        (makeFields, Lens)
import           Data.Aeson.TH       (defaultOptions, deriveJSON)
import           Data.Serialize      (Serialize (..))
import           Data.Serialize.Text ()

import Bio.Data.Experiment.File

data Replicate file = Replicate
    { replicateFiles  :: file
    , _replicateInfo   :: (Map T.Text T.Text)
    , _replicateNumber :: Int
    }
    deriving (Generic)

makeFields ''Replicate

files :: Lens (Replicate file1) (Replicate file2) file1 file2
files f s = (\x -> s{replicateFiles = x}) <$> f (replicateFiles s)
{-# INLINE files #-}

deriveJSON defaultOptions ''Replicate
instance Serialize file => Serialize (Replicate file)
