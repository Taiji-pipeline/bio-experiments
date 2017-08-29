{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Bio.Data.Experiment
    ( FileType(..)
    , FileTag(..)

    , File
    , location
    , tags
    , emptyFile

    , SomeFile(..)
    , fromSomeFile
    , SomeTags(..)
    , FileList(..)

    , FileTagInfo(..)
    , FileTypeInfo(..)

    , Replicate(..)
    , files
    , info
    , number
    , Experiment(..)
    , S
    , N

    , ATACSeq
    , RNASeq

    , AllC
    , Splitter(..)
    , Merger(..)
    ) where

import           Control.Lens
import           Data.Function                 (on)
import           Data.List                     (groupBy, sortBy)
import qualified Data.Map.Strict               as M
import           Data.Monoid                   ((<>))
import           Data.Ord                      (comparing)
import           GHC.Exts                      (Constraint)

import           Bio.Data.Experiment.ATACSeq
import           Bio.Data.Experiment.File
import           Bio.Data.Experiment.Replicate
import           Bio.Data.Experiment.RNASeq
import           Bio.Data.Experiment.Types

emptyFile :: File tags filetype
emptyFile = File
    { fileLocation = ""
    , fileInfo = M.empty
    , fileTags = []
    }

type family AllC (c :: k -> Constraint) (xs :: [k]) :: Constraint where
    AllC c '[] = ()
    AllC c (x ': xs) = (c x, AllC c xs)

class Splitter a where
    type SplitResult a :: *
    split :: a -> [SplitResult a]

instance Splitter (Replicate [f]) where
    type SplitResult (Replicate [f]) = Replicate f
    split r = zipWith (\x y -> files .~ x $ y) (r^.files) $ repeat r

instance Experiment e => Splitter (e N [f]) where
    type SplitResult (e N [f]) = e S f
    split e = zipWith (\x y -> replicates .~ return x $ y)
        (concatMap split $ e^.replicates) $ repeat e

instance Experiment e => Splitter (e S (Either f1 f2)) where
    type SplitResult (e S (Either f1 f2)) = Either (e S f1) (e S f2)
    split e = case runIdentity (e^.replicates) ^. files of
        Left fl -> [Left $ replicates.mapped.files .~ fl $ e]
        Right fl -> [Right $ replicates.mapped.files .~ fl $ e]

{-
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
-}

class Merger a where
    type MergeResult a :: *
    merge :: [a] -> [MergeResult a]

instance Merger (Replicate f) where
    type MergeResult (Replicate f) = Replicate [f]
    merge rs = map combineRep repGroup
      where
        repGroup = groupBy ((==) `on` (^.number)) $ sortBy (comparing (^.number)) rs
        combineRep reps = files .~ map (^.files) reps $ info .~ infos $ head reps
          where
            infos = M.fromListWith f $ concatMap (M.toList . (^.info)) reps
            f a b | a == b = a
                  | otherwise = a <> " | " <> b

instance Experiment e => Merger (e S f) where
    type MergeResult (e S f) = e N [f]
    merge = map combineExp . groupBy ((==) `on` (^.eid)) .
        sortBy (comparing (^.eid))
      where
        combineExp expGrp =
            if allEqual (map (\x -> (x^.groupName, x^.sampleName)) expGrp)
                then replicates .~ merge
                    (map (runIdentity . (^.replicates)) expGrp) $ head expGrp
                else error "Abort: Found experiments with same id but with different contents"
        allEqual (x:xs) = all (==x) xs
        allEqual _      = True

{-
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
-}
