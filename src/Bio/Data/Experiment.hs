{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
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
    , number
    , Experiment(..)
    , S
    , N

    , ATACSeq
    , RNASeq
    , HiC

    , AllC
    , Insert'
    , Splitter(..)
    , Merger(..)
    , zipExp
    ) where

import           Control.Lens
import           Data.Function                 (on)
import           Data.List                     (groupBy, sortBy)
import qualified Data.Map.Strict               as M
import           Data.Maybe                    (mapMaybe)
import           Data.Monoid                   ((<>))
import           Data.Ord                      (comparing)
import           Data.Promotion.Prelude.List   (Elem, Insert)
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
    split e = zipWith (\x y -> replicates .~ return x $ y)
        (e^.replicates) $ repeat e
    {-# INLINE split #-}

instance Experiment e => Splitter (e S [f]) (e S f) where
    split e = zipWith (\x y -> replicates .~ return x $ y)
        (split $ runIdentity $ e^.replicates) $ repeat e
    {-# INLINE split #-}

instance Experiment e => Splitter (e S (Either f1 f2)) (Either (e S f1) (e S f2)) where
    split e = case runIdentity (e^.replicates) ^. files of
        Left fl  -> [Left $ replicates.mapped.files .~ fl $ e]
        Right fl -> [Right $ replicates.mapped.files .~ fl $ e]
    {-# INLINE split #-}

class Merger a b | a -> b where
    merge :: [a] -> [b]

instance Merger (Replicate f) (Replicate [f]) where
    merge rs = map combineRep repGroup
      where
        repGroup = groupBy ((==) `on` (^.number)) $ sortBy (comparing (^.number)) rs
        combineRep reps = files .~ map (^.files) reps $ info .~ infos $ head reps
          where
            infos = M.fromListWith f $ concatMap (M.toList . (^.info)) reps
            f a b | a == b = a
                  | otherwise = a <> " | " <> b
    {-# INLINE merge #-}

instance Experiment e => Merger (e S f) (e N [f]) where
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
    {-# INLINE merge #-}

zipExp :: Experiment e
       => [e S file1]
       -> [e S file2]
       -> [e S (file1, file2)]
zipExp xs ys = flip mapMaybe xs $ \x ->
    let k = (x^.eid, runIdentity (x^.replicates) ^. number)
    in zip' x <$> M.lookup k ys'
  where
    ys' = M.fromListWithKey raiseErr $ map (\x ->
        ( (x^.eid, runIdentity (x^.replicates) ^. number)
        , runIdentity (x^.replicates) ^. files )
        ) ys
    raiseErr k _ _ = error $ "Duplicate records: " ++ show k
    zip' e x = e & replicates.mapped.files %~ (\f -> (f, x))
{-# INLINE zipExp #-}
