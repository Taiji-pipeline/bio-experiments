{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Bio.Data.Experiment
    ( FileType(..)
    , File
    , location
    , tags
    , emptyFile

    , MaybeTagged
    , untagMaybe
    , MaybePaired
    , isPaired

    , Replicate(..)
    , files
    , info
    , number
    , Experiment(..)

    , ATACSeq
    , RNASeq

    , Tag(..)
    , elemTag
    , MayHave
    , Elem
    , Insert
    , Remove
    ) where

import qualified Data.Map.Strict as M
import Data.Typeable
import Data.Type.Bool
import Data.Type.Equality
import Data.Tagged (Tagged(..))

import Bio.Data.Experiment.RNASeq
import Bio.Data.Experiment.ATACSeq
import Bio.Data.Experiment.File
import Bio.Data.Experiment.Types
import Bio.Data.Experiment.Replicate

data Tag = Sorted
         | Pairend
         | GZipped

type MayHave tag tags = Typeable (Elem tag tags)

type family Elem (x :: Tag) (xs :: [Tag]) :: Bool where
    Elem _ '[] = 'False
    Elem a (x ': xs) = a == x || Elem a xs

type family Insert (x :: Tag) (xs :: [Tag]) :: [Tag] where
    Insert a xs = (a ': xs)

type family Remove (x :: Tag) (xs :: [Tag]) :: [Tag] where
    Remove _ '[] = '[]
    Remove a (x ': xs) = If (a == x) xs (x ': Remove a xs)

elemTag :: forall a (tag :: Tag) (tags :: [Tag]) . Typeable (Elem tag tags)
        => Proxy tag
        -> Tagged tags a -> Bool
elemTag _ _
    | typeOf (Proxy :: Proxy (Elem tag tags)) ==
      typeOf (Proxy :: Proxy 'True) = True
    | otherwise = False

type MaybePaired f = Either f (f, f)

isPaired :: MaybePaired f -> Bool
isPaired (Left _) = False
isPaired (Right _) = True
{-# INLINE isPaired #-}

emptyFile :: File filetype
emptyFile = File
    { fileLocation = ""
    , fileInfo = M.empty
    , fileTags = []
    }

{-
class NGS experiment where
    pair
    -}
