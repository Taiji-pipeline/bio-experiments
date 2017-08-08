{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TemplateHaskell #-}

module Bio.Data.Experiment
    ( FileType(..)
    , FileTag(..)
    , File
    , SomeFile(..)
    , SomeTags(..)
    , FileList(..)
    , location
    , tags
    , emptyFile

    , MaybePair

    , Replicate(..)
    , files
    , info
    , number
    , Experiment(..)

    , ATACSeq
    , RNASeq

    , IsGzip
    , IsGzipSym0
    , hasTag

    , AllC
    , splitExpByFile
    , getFileType
    ) where

import qualified Data.Map.Strict as M
import Data.Typeable (Typeable, Proxy(..), typeOf)
import Data.Promotion.Prelude.List (Elem)
import GHC.Exts (Constraint)
import Data.Promotion.TH
import Data.Singletons.Prelude.List hiding (Replicate)
import Data.Singletons
import Control.Lens

import Bio.Data.Experiment.RNASeq
import Bio.Data.Experiment.ATACSeq
import Bio.Data.Experiment.File
import Bio.Data.Experiment.Types
import Bio.Data.Experiment.Replicate

hasTag :: forall tags filetype . SingI tags
       => File tags filetype -> FileTag -> Bool
hasTag _ t = t `elem` fromSing (sing :: SList tags)

emptyFile :: File tags filetype
emptyFile = File
    { fileLocation = ""
    , fileInfo = M.empty
    , fileTags = []
    }

$(promote [d|
    isGzip :: FileTag -> Bool
    isGzip Gzip = True
    isGzip _ = False
    |])

type family AllC (c :: k -> Constraint) (xs :: [k]) :: Constraint where
    AllC c '[] = ()
    AllC c (x ': xs) = (c x, AllC c xs)

-- | Split a single experiment into multiple experiments, each containing a
-- fileset.
splitExpByFile :: Experiment e => e [f] -> [e f]
splitExpByFile e = zipWith (\x y -> replicates .~ [x] $ y)
    (concatMap f $ e^.replicates) $ repeat e
  where
    f r = zipWith (\x y -> files .~ x $ y) (r^.files) $ repeat r
{-# INLINE splitExpByFile #-}
