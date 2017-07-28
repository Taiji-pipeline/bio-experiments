{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module Bio.Data.Experiment.ATACSeq where

import           Data.Aeson.TH       (defaultOptions, deriveJSON)
import           GHC.Generics        (Generic)
import           Data.Serialize      (Serialize (..))
import           Data.Serialize.Text ()

import           Bio.Data.Experiment.Types
import           Bio.Data.Experiment.File

data ATACSeq file = ATACSeq
    { atacseqCommon    :: CommonFields file
    , atacseqPairedEnd :: !Bool
    } deriving (Eq, Ord, Generic)

deriveJSON defaultOptions ''ATACSeq
instance Serialize file => Serialize (ATACSeq file)

instance BioData file ~ 'True => Experiment ATACSeq file where
    commonFields f e = (\x -> e{atacseqCommon = x}) <$> f (atacseqCommon e)
