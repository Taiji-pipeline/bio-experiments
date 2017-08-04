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
    , FileTag(..)
    , File
    , location
    , tags
    , emptyFile

    , MaybePaired

    , Replicate(..)
    , files
    , info
    , number
    , Experiment(..)

    , ATACSeq
    , RNASeq

    , elemTag
    , MayHave
    , Elem
    , Insert
    , Remove
    , isGzipped
    , isPairend
    ) where

import qualified Data.Map.Strict as M
import Data.Typeable
import Data.Type.Bool
import Data.Type.Equality (type (==))

import Bio.Data.Experiment.RNASeq
import Bio.Data.Experiment.ATACSeq
import Bio.Data.Experiment.File
import Bio.Data.Experiment.Types
import Bio.Data.Experiment.Replicate

-- | Define type equality instance
type family EqTag (a :: FileTag) (b :: FileTag) where
    EqTag a a = 'True
    EqTag a b = 'False

type instance a == b = EqTag a b

type MayHave tag tags = Typeable (Elem tag tags)

type family Elem (x :: FileTag) (xs :: [FileTag]) :: Bool where
    Elem _ '[] = 'False
    Elem a (x ': xs) = a == x || Elem a xs

type family Insert (x :: FileTag) (xs :: [FileTag]) :: [FileTag] where
    Insert a xs = (a ': xs)

type family Remove (x :: FileTag) (xs :: [FileTag]) :: [FileTag] where
    Remove _ '[] = '[]
    Remove a (x ': xs) = If (a == x) xs (x ': Remove a xs)

elemTag :: forall a (tag :: FileTag) (tags :: [FileTag]) . Typeable (Elem tag tags)
        => Proxy tag
        -> File tags a -> Bool
elemTag _ _
    | typeOf (Proxy :: Proxy (Elem tag tags)) ==
      typeOf (Proxy :: Proxy 'True) = True
    | otherwise = False

isGzipped :: MayHave 'GZipped tags => File tags a -> Bool
isGzipped = elemTag (Proxy :: Proxy 'GZipped)

type MaybePaired f = Either f (f, f)

isPairend :: MayHave 'Pairend tags => File tags a -> Bool
isPairend = elemTag (Proxy :: Proxy 'Pairend)
{-# INLINE isPairend #-}

emptyFile :: File tags filetype
emptyFile = File
    { fileLocation = ""
    , fileInfo = M.empty
    , fileTags = []
    }

{-
class NGS experiment where
    pair
    -}
