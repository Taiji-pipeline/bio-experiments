{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Bio.Data.Experiment.Types where

import           Control.Lens                  (makeLenses, Lens')
import           Data.Aeson.TH       (defaultOptions, deriveJSON)
import           Data.Serialize      (Serialize (..))
import           Data.Serialize.Text ()
import qualified Data.Text                     as T
import           GHC.Generics                  (Generic)

import           Bio.Data.Experiment.File
import           Bio.Data.Experiment.Replicate

-- | A set of fields that exist in all kinds of Assays
data CommonFields file = CommonFields
    { _commonEid        :: !T.Text
    , _commonGroupName  :: !(Maybe T.Text)
    , _commonSampleName :: !T.Text
    , _commonReplicates :: [Replicate file]
    } deriving (Ord, Eq, Generic)

makeLenses ''CommonFields
deriveJSON defaultOptions ''CommonFields
instance Serialize file => Serialize (CommonFields file)

defaultCommonFields :: CommonFields file
defaultCommonFields = CommonFields
    { _commonEid = ""
    , _commonGroupName = Nothing
    , _commonSampleName = ""
    , _commonReplicates = []
    }

class BioData file ~ 'True => Experiment e file where
    commonFields :: Lens' (e file) (CommonFields file)

    eid :: Lens' (e file) T.Text
    eid = commonFields . commonEid

    groupName :: Lens' (e file) (Maybe T.Text)
    groupName = commonFields . commonGroupName

    sampleName :: Lens' (e file) T.Text
    sampleName = commonFields . commonSampleName

    replicates :: Lens' (e file) [Replicate file]
    replicates = commonFields . commonReplicates

    {-# MINIMAL commonFields #-}
