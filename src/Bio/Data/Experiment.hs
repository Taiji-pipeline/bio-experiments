{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Bio.Data.Experiment
    ( FileType(..)
    , FileTag(..)
    , File
    , FilePair
    , MaybePair
    , EitherTag
    , SomeFile(..)
    , SomeTags(..)
    , FileList(..)
    , location
    , tags
    , emptyFile

    , Replicate(..)
    , files
    , info
    , number
    , Experiment(..)
    , MaybePairExp

    , ATACSeq
    , RNASeq

    , hasTag

    , AllC
    , splitExpByFile
    , splitExpByFileEither
    , getFileType
    , mergeExps
    ) where

import           Control.Lens
import           Data.Bifunctor                (bimap)
import           Data.Either                   (partitionEithers)
import           Data.Function                 (on)
import           Data.List                     (groupBy, sortBy)
import qualified Data.Map.Strict               as M
import           Data.Monoid                   ((<>))
import           Data.Ord                      (comparing)
import           Data.Singletons
import           Data.Singletons.Prelude.List  hiding (Replicate)
import           GHC.Exts                      (Constraint)

import           Bio.Data.Experiment.ATACSeq
import           Bio.Data.Experiment.File
import           Bio.Data.Experiment.Replicate
import           Bio.Data.Experiment.RNASeq
import           Bio.Data.Experiment.Types

hasTag :: forall tags filetype . SingI tags
       => File tags filetype -> FileTag -> Bool
hasTag _ t = t `elem` fromSing (sing :: SList tags)

emptyFile :: File tags filetype
emptyFile = File
    { fileLocation = ""
    , fileInfo = M.empty
    , fileTags = []
    }

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

splitExpByFileEither :: Experiment e
                     => e [Either f1 f2]
                     -> Maybe [Either (e f1) (e f2)]
splitExpByFileEither e = if not (null lf) && not (null rt)
    then Nothing
    else Just $ map (bimap (\x -> replicates .~ [x] $ e)
        (\x -> replicates .~ [x] $ e)) reps
  where
    (lf, rt) = partitionEithers reps
    reps = concatMap f $ e^.replicates
    f :: Replicate [Either f1 f2] -> [Either (Replicate f1) (Replicate f2)]
    f r = map (bimap (\x -> files .~ x $ r) (\x -> files .~ x $ r)) $ r^.files
{-# INLINE splitExpByFileEither #-}

-- | Merge experiments with same id.
mergeExps :: Experiment e => [e f] -> [e [f]]
mergeExps es = map combineExp expGroup
  where
    expGroup = groupBy ((==) `on` (^.eid)) $ sortBy (comparing (^.eid)) es
    combineExp e = if allEqual (map (\x -> (x^.groupName, x^.sampleName)) e)
        then replicates .~ mergeReps (concatMap (^.replicates) e) $ head e
        else error "Abort: Found experiments with same id but with different contents"
    allEqual (x:xs) = all (==x) xs
    allEqual _      = True
{-# INLINE mergeExps #-}

-- | Merge replicates with same number.
mergeReps :: [Replicate f] -> [Replicate [f]]
mergeReps rs = map combineRep repGroup
  where
    repGroup = groupBy ((==) `on` (^.number)) $ sortBy (comparing (^.number)) rs
    combineRep reps = files .~ map (^.files) reps $ info .~ infos $ head reps
      where
        infos = M.fromListWith f $ concatMap (M.toList . (^.info)) reps
        f a b | a == b = a
              | otherwise = a <> " | " <> b
{-# INLINE mergeReps #-}
