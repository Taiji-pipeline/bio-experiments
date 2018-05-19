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

module Bio.Data.Experiment.Types where

import           Control.Lens                  (Lens, Lens', makeLenses)
import           Data.Aeson
import qualified Data.IntMap.Strict            as IM
import           Data.Serialize                (Serialize (..))
import           Data.Serialize.Text           ()
import qualified Data.Text                     as T
import           GHC.Generics                  (Generic)

import           Bio.Data.Experiment.Replicate

type S = (,) Int
type N = IM.IntMap

-- | A set of fields that exist in all kinds of Assays
data CommonFields container file = CommonFields
    { _commonEid        :: !T.Text
    , _commonGroupName  :: !(Maybe T.Text)
    , _commonSampleName :: !T.Text
    , _commonReplicates :: container (Replicate file)
    } deriving (Generic)

makeLenses ''CommonFields

instance FromJSON (container (Replicate file))
    => FromJSON (CommonFields container file)

instance ToJSON (container (Replicate file))
    => ToJSON (CommonFields container file)

instance Serialize (container (Replicate file)) => Serialize (CommonFields container file)

defaultCommonFields :: CommonFields N file
defaultCommonFields = CommonFields
    { _commonEid = ""
    , _commonGroupName = Nothing
    , _commonSampleName = ""
    , _commonReplicates = IM.empty
    }

class Experiment e where
    commonFields :: Lens (e container1 file1)
                         (e container2 file2)
                         (CommonFields container1 file1)
                         (CommonFields container2 file2)

    eid :: Lens' (e c file1) T.Text
    eid = commonFields . commonEid

    groupName :: Lens' (e c file1) (Maybe T.Text)
    groupName = commonFields . commonGroupName

    sampleName :: Lens' (e c file1) T.Text
    sampleName = commonFields . commonSampleName

    replicates :: Lens (e container1 file1)
                       (e container2 file2)
                       (container1 (Replicate file1))
                       (container2 (Replicate file2))
    replicates = commonFields . commonReplicates

    {-# MINIMAL commonFields #-}


-- | ATAC-seq
data ATACSeq container file = ATACSeq
    { atacseqCommon    :: CommonFields container file
    } deriving (Generic)

instance FromJSON (CommonFields container file)
    => FromJSON (ATACSeq container file)

instance ToJSON (CommonFields container file)
    => ToJSON (ATACSeq container file)

instance Serialize (CommonFields container file) => Serialize (ATACSeq container file)

instance Experiment ATACSeq where
    commonFields f e = (\x -> e{atacseqCommon = x}) <$> f (atacseqCommon e)

-- | ChIP-seq
data ChIPSeq container file = ChIPSeq
    { chipseqCommon    :: CommonFields container file
    } deriving (Generic)

instance FromJSON (CommonFields container file)
    => FromJSON (ChIPSeq container file)

instance ToJSON (CommonFields container file)
    => ToJSON (ChIPSeq container file)

instance Serialize (CommonFields container file) => Serialize (ChIPSeq container file)

instance Experiment ChIPSeq where
    commonFields f e = (\x -> e{ chipseqCommon = x }) <$> f (chipseqCommon e)

-- | RNA-seq
data RNASeq container file = RNASeq
    { rnaseqCommon    :: CommonFields container file
    } deriving (Generic)

instance FromJSON (CommonFields container file)
    => FromJSON (RNASeq container file)

instance ToJSON (CommonFields container file)
    => ToJSON (RNASeq container file)

instance Serialize (CommonFields container file) => Serialize (RNASeq container file)

instance Experiment RNASeq where
    commonFields f e = (\x -> e{rnaseqCommon = x}) <$> f (rnaseqCommon e)


-- | HiC
data HiC container file = HiC
    { hicCommon    :: CommonFields container file
    } deriving (Generic)

instance FromJSON (CommonFields container file)
    => FromJSON (HiC container file)

instance ToJSON (CommonFields container file)
    => ToJSON (HiC container file)

instance Serialize (CommonFields container file) => Serialize (HiC container file)

instance Experiment HiC where
    commonFields f e = (\x -> e{hicCommon = x}) <$> f (hicCommon e)
