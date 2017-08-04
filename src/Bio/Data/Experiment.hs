{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
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
    , isGzipped
    , isPairend
    ) where

import qualified Data.Map.Strict as M
import Data.Typeable (Typeable, Proxy(..), typeOf)
import Data.Promotion.Prelude.List (Elem)

import Bio.Data.Experiment.RNASeq
import Bio.Data.Experiment.ATACSeq
import Bio.Data.Experiment.File
import Bio.Data.Experiment.Types
import Bio.Data.Experiment.Replicate

type MayHave tag tags = Typeable (Elem tag tags)

elemTag :: forall a (tag :: FileTag) (tags :: [FileTag]) . Typeable (Elem tag tags)
        => Proxy tag
        -> File tags a -> Bool
elemTag _ _
    | typeOf (Proxy :: Proxy (Elem tag tags)) ==
      typeOf (Proxy :: Proxy 'True) = True
    | otherwise = False

isGzipped :: MayHave 'Gzip tags => File tags a -> Bool
isGzipped = elemTag (Proxy :: Proxy 'Gzip)

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
