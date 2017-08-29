{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE UndecidableInstances   #-}
module Bio.Data.Experiment.RNASeq where

import           Data.Aeson
import           Data.Serialize                (Serialize (..))
import           Data.Serialize.Text           ()
import           GHC.Generics                  (Generic)

import           Bio.Data.Experiment.Types

data RNASeq container file = RNASeq
    { rnaseqCommon    :: CommonFields container file
    , rnaseqPairedEnd :: !Bool
    } deriving (Generic)

instance FromJSON (CommonFields container file)
    => FromJSON (RNASeq container file)

instance ToJSON (CommonFields container file)
    => ToJSON (RNASeq container file)

instance Serialize (CommonFields container file) => Serialize (RNASeq container file)

instance Experiment RNASeq where
    commonFields f e = (\x -> e{rnaseqCommon = x}) <$> f (rnaseqCommon e)
