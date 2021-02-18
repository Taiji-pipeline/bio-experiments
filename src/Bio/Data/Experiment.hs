{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}

module Bio.Data.Experiment
    ( FileType(..)
    , FileTag(..)

    , File
    , location
    , emptyFile

    , SomeFile(..)
    , fromSomeFile
    , SomeTags(..)
    , fromSomeTags
    , castFile

    , FileTagInfo(..)
    , FileTypeInfo(..)

    , Replicate(..)
    , files
    , info
    , Experiment
    , S
    , N
    , eid
    , groupName
    , sampleName
    , replicates

    , ATACSeq
    , ChIPSeq
    , RNASeq
    , HiC

    , AllC
    , Insert'
    , Splitter(..)
    , mergeExp
    , zipExp
    ) where

import           Control.Arrow                 (second)
import           Lens.Micro
import           Data.Function                 (on)
import qualified Data.IntMap.Strict            as IM
import           Data.List                     (groupBy, sortBy)
import qualified Data.Map.Strict               as M
import           Data.Ord                      (comparing)
import           Data.Singletons.Prelude.List   (Elem, Insert)
import           Data.Singletons.Prelude.Bool  (If)
import           GHC.Exts                      (Constraint)

import           Bio.Data.Experiment.File
import           Bio.Data.Experiment.Replicate
import           Bio.Data.Experiment.Types

emptyFile :: File tags filetype
emptyFile = File
    { fileLocation = ""
    , fileInfo = M.empty
    }

type family AllC (c :: k -> Constraint) (xs :: [k]) :: Constraint where
    AllC c '[] = ()
    AllC c (x ': xs) = (c x, AllC c xs)

type Insert' x xs = If (Elem x xs) xs (Insert x xs)

class Splitter a b | a -> b where
    split :: a -> [b]

instance Splitter (Replicate [f]) (Replicate f) where
    split r = zipWith (\x y -> files .~ x $ y) (r^.files) $ repeat r
    {-# INLINE split #-}

instance Experiment e => Splitter (e N f) (e S f) where
    split e = zipWith (\x y -> replicates .~ x $ y)
        (IM.toList $ e^.replicates) $ repeat e
    {-# INLINE split #-}

instance Experiment e => Splitter (e S [f]) (e S f) where
    split e = zipWith (\x y -> replicates .~ x $ y) reps $ repeat e
      where
        reps = let (i, x) = e^.replicates
               in zip (repeat i) $ split x
    {-# INLINE split #-}

mergeExp :: Experiment e => [e S f] -> [e N [f]]
mergeExp = map combineExp . groupBy ((==) `on` (^.eid)) .
    sortBy (comparing (^.eid))
  where
    combineExp expGrp =
        if allEqual (map (\x -> (x^.groupName, x^.sampleName)) expGrp)
            then replicates .~ mergeReplicates
                (map (^.replicates) expGrp) $ head expGrp
            else error "Abort: Found experiments with same id but with different contents"
    allEqual (x:xs) = all (==x) xs
    allEqual _      = True
{-# INLINE mergeExp #-}

mergeReplicates :: [S (Replicate f)] -> N (Replicate [f])
mergeReplicates = IM.fromListWith f . map (second (\x -> files %~ return $ x))
      where
        f r1 r2 = files .~ fls $ info .~ infos $ r1
          where
            fls = r1^.files ++ r2^.files
            infos = M.unionWith g (r1^.info) $ r2^.info
            g a b | a == b = a
                  | otherwise = a <> " | " <> b
{-# INLINE mergeReplicates #-}

zipExp :: Experiment e
       => [e S file1]
       -> [e S file2]
       -> [e S (file1, file2)]
zipExp xs ys = M.elems $ M.intersectionWith f xs' ys'
  where
    f x y = x & replicates._2.files %~ (\f1 -> (f1, y^.replicates._2.files))
    xs' = M.fromListWithKey raiseErr $
        map ( \x -> ((x^.eid, x^.replicates._1), x) ) xs
    ys' = M.fromListWithKey raiseErr $
        map ( \x -> ((x^.eid, x^.replicates._1), x) ) ys
    raiseErr k _ _ = error $ "Duplicate records: " ++ show k
{-# INLINE zipExp #-}
