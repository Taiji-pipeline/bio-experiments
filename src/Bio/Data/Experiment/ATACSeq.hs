{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE UndecidableInstances   #-}
module Bio.Data.Experiment.ATACSeq where

import           Data.Aeson
import           Data.Serialize            (Serialize (..))
import           Data.Serialize.Text       ()
import           GHC.Generics              (Generic)

import           Bio.Data.Experiment.Types

data ATACSeq container file = ATACSeq
    { atacseqCommon    :: CommonFields container file
    , atacseqPairedEnd :: !Bool
    } deriving (Generic)

instance FromJSON (CommonFields container file)
    => FromJSON (ATACSeq container file)

instance ToJSON (CommonFields container file)
    => ToJSON (ATACSeq container file)

instance Serialize (CommonFields container file) => Serialize (ATACSeq container file)

instance Experiment ATACSeq where
    commonFields f e = (\x -> e{atacseqCommon = x}) <$> f (atacseqCommon e)
