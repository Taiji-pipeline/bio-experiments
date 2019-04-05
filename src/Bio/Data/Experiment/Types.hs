{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bio.Data.Experiment.Types
    ( S
    , N
    , Experiment
    , eid
    , groupName
    , sampleName
    , replicates
    , CommonFields(..)
    , defaultCommonFields
    , ATACSeq(..)
    , RNASeq(..)
    , ChIPSeq(..)
    , HiC(..)
    ) where

import           Control.Lens                  (Lens, Lens', makeLenses)
import           Data.Aeson
import qualified Data.IntMap.Strict            as IM
import           Data.Serialize                (Serialize (..))
import           Data.Serialize.Text           ()
import qualified Data.Text                     as T
import           GHC.Generics                  (Generic)

import           Bio.Data.Experiment.Replicate

-- | Containers
type S = (,) Int
type N = IM.IntMap

class Experiment e where
    getFields :: e c f -> CommonFields c f
    setFields :: CommonFields c1 f1 -> e c2 f2 -> e c1 f1


-- | A set of fields that exist in all kinds of Assays
data CommonFields container file = CommonFields
    { _commonEid        :: !T.Text
    , _commonGroupName  :: !(Maybe T.Text)
    , _commonSampleName :: !T.Text
    , _commonReplicates :: container (Replicate file)
    } deriving (Generic)

defaultCommonFields :: CommonFields N file
defaultCommonFields = CommonFields
    { _commonEid = ""
    , _commonGroupName = Nothing
    , _commonSampleName = ""
    , _commonReplicates = IM.empty
    }

makeLenses ''CommonFields

instance FromJSON (container (Replicate file)) =>
    FromJSON (CommonFields container file)

instance ToJSON (container (Replicate file)) =>
    ToJSON (CommonFields container file)

instance Serialize (container (Replicate file)) =>
    Serialize (CommonFields container file)

instance Experiment CommonFields where
    getFields = id
    setFields f _ = f

commonFields :: Experiment e => Lens (e container1 file1)
                        (e container2 file2)
                        (CommonFields container1 file1)
                        (CommonFields container2 file2)
commonFields f e = (\x -> setFields x e) <$> f (getFields e)

eid :: Experiment e => Lens' (e c file1) T.Text
eid = commonFields . commonEid

groupName :: Experiment e => Lens' (e c file1) (Maybe T.Text)
groupName = commonFields . commonGroupName

sampleName :: Experiment e => Lens' (e c file1) T.Text
sampleName = commonFields . commonSampleName

replicates :: Experiment e => Lens (e container1 file1)
    (e container2 file2) (container1 (Replicate file1))
    (container2 (Replicate file2))
replicates = commonFields . commonReplicates


-- | ATAC-seq
newtype ATACSeq container file = ATACSeq
    { atacseqCommon    :: CommonFields container file
    } deriving (Generic, Experiment)

instance FromJSON (CommonFields container file) =>
    FromJSON (ATACSeq container file)

instance ToJSON (CommonFields container file) =>
    ToJSON (ATACSeq container file)

instance Serialize (CommonFields container file) =>
    Serialize (ATACSeq container file)

-- | ChIP-seq
newtype ChIPSeq container file = ChIPSeq
    { chipseqCommon    :: CommonFields container file
    } deriving (Generic, Experiment)

instance FromJSON (CommonFields container file)
    => FromJSON (ChIPSeq container file)

instance ToJSON (CommonFields container file)
    => ToJSON (ChIPSeq container file)

instance Serialize (CommonFields container file) => Serialize (ChIPSeq container file)

-- | RNA-seq
newtype RNASeq container file = RNASeq
    { rnaseqCommon    :: CommonFields container file
    } deriving (Generic, Experiment)

instance FromJSON (CommonFields container file)
    => FromJSON (RNASeq container file)

instance ToJSON (CommonFields container file)
    => ToJSON (RNASeq container file)

instance Serialize (CommonFields container file) => Serialize (RNASeq container file)

-- | HiC
newtype HiC container file = HiC
    { hicCommon    :: CommonFields container file
    } deriving (Generic, Experiment)

instance FromJSON (CommonFields container file)
    => FromJSON (HiC container file)

instance ToJSON (CommonFields container file)
    => ToJSON (HiC container file)

instance Serialize (CommonFields container file) => Serialize (HiC container file)
