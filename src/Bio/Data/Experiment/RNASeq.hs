{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module Bio.Data.Experiment.RNASeq where

import           Data.Aeson.TH       (defaultOptions, deriveJSON)
import           GHC.Generics        (Generic)
import           Data.Serialize      (Serialize (..))
import           Data.Serialize.Text ()

import           Bio.Data.Experiment.Types
import           Bio.Data.Experiment.File

data RNASeq file = RNASeq
    { rnaseqCommon    :: CommonFields file
    , rnaseqPairedEnd :: !Bool
    } deriving (Ord, Eq, Generic)

deriveJSON defaultOptions ''RNASeq
instance Serialize file => Serialize (RNASeq file)

instance BioData file ~ 'True => Experiment RNASeq file where
    commonFields f e = (\x -> e{rnaseqCommon = x}) <$> f (rnaseqCommon e)
