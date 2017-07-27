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
import           Control.Lens        (makeFields)
import           Data.Aeson.TH       (defaultOptions, deriveJSON)
import           Data.Serialize      (Serialize (..))
import           Data.Serialize.Text ()

import Bio.Data.Experiment.File

data Replicate file where
    Replicate :: { replicateFiles  :: file
                 , replicateInfo   :: (Map T.Text T.Text)
                 , replicateNumber :: Int
                 } -> Replicate file
                 deriving (Ord, Eq, Generic)

makeFields ''Replicate

deriveJSON defaultOptions ''Replicate
instance Serialize file => Serialize (Replicate file)
