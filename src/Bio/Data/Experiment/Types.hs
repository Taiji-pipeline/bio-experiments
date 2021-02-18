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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Bio.Data.Experiment.Types
    ( S
    , N
    , Experiment
    , eid
    , groupName
    , sampleName
    , replicates
    , batch
    , CommonFields(..)
    , defaultCommonFields
    , ATACSeq(..)
    , RNASeq(..)
    , ChIPSeq(..)
    , HiC(..)
    ) where

import           Lens.Micro.TH (makeLenses)
import           Lens.Micro (Lens, Lens')
import qualified Data.IntMap.Strict            as IM
import           Data.Binary (Binary(..))
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
    , _commonBatch      :: ![T.Text]
    } deriving (Generic)

defaultCommonFields :: CommonFields N file
defaultCommonFields = CommonFields
    { _commonEid = ""
    , _commonGroupName = Nothing
    , _commonSampleName = ""
    , _commonReplicates = IM.empty
    , _commonBatch      = []
    }

makeLenses ''CommonFields

deriving instance Show (container (Replicate file)) =>
    Show (CommonFields container file)

instance Binary (container (Replicate file)) =>
    Binary (CommonFields container file)

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
{-# INLINE eid #-}

groupName :: Experiment e => Lens' (e c file1) (Maybe T.Text)
groupName = commonFields . commonGroupName
{-# INLINE groupName #-}

sampleName :: Experiment e => Lens' (e c file1) T.Text
sampleName = commonFields . commonSampleName
{-# INLINE sampleName #-}

replicates :: Experiment e => Lens (e container1 file1)
    (e container2 file2) (container1 (Replicate file1))
    (container2 (Replicate file2))
replicates = commonFields . commonReplicates
{-# INLINE replicates #-}

batch :: Experiment e => Lens' (e c file1) [T.Text]
batch = commonFields . commonBatch
{-# INLINE batch #-}

-- | ATAC-seq
newtype ATACSeq container file = ATACSeq
    { atacseqCommon    :: CommonFields container file
    } deriving (Generic, Experiment)

deriving instance Show (CommonFields container file) =>
    Show (ATACSeq container file)

instance Binary (CommonFields container file) =>
    Binary (ATACSeq container file)

-- | ChIP-seq
newtype ChIPSeq container file = ChIPSeq
    { chipseqCommon    :: CommonFields container file
    } deriving (Generic, Experiment)

deriving instance Show (CommonFields container file)
    => Show (ChIPSeq container file)

instance Binary (CommonFields container file) => Binary (ChIPSeq container file)

-- | RNA-seq
newtype RNASeq container file = RNASeq
    { rnaseqCommon    :: CommonFields container file
    } deriving (Generic, Experiment)

deriving instance Show (CommonFields container file)
    => Show (RNASeq container file)

instance Binary (CommonFields container file) => Binary (RNASeq container file)

-- | HiC
newtype HiC container file = HiC
    { hicCommon    :: CommonFields container file
    } deriving (Generic, Experiment)

deriving instance Show (CommonFields container file)
    => Show (HiC container file)

instance Binary (CommonFields container file) => Binary (HiC container file)
