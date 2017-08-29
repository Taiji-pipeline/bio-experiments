{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Bio.Data.Experiment.Types where

import           Control.Lens                  (makeLenses, Lens', Lens)
import           Data.Aeson
import           Data.Serialize      (Serialize (..))
import           Data.Serialize.Text ()
import qualified Data.Text                     as T
import           GHC.Generics                  (Generic)
import Data.Functor.Identity

import           Bio.Data.Experiment.Replicate

type S = Identity
type N = []

instance Serialize a => Serialize (Identity a) where
    put (Identity x) = put x
    get = Identity <$> get

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
    , _commonReplicates = []
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
