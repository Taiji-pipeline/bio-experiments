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
    , FileList(..)
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

    , IsGzip
    , IsGzipSym0
    , hasTag

    , AllC
    ) where

import qualified Data.Map.Strict as M
import Data.Typeable (Typeable, Proxy(..), typeOf)
import Data.Promotion.Prelude.List (Elem)
import GHC.Exts (Constraint)
import Data.Promotion.TH
import Data.Singletons.Prelude.List hiding (Replicate)
import Data.Singletons

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

hasTag :: forall tags filetype . SingI tags
       => File tags filetype -> FileTag -> Bool
hasTag _ t = t `elem` fromSing (sing :: SList tags)

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

$(promote [d|
    isGzip :: FileTag -> Bool
    isGzip Gzip = True
    isGzip _ = False
    |])

type family AllC (c :: k -> Constraint) (xs :: [k]) :: Constraint where
    AllC c '[] = ()
    AllC c (x ': xs) = (c x, AllC c xs)

{-
class NGS experiment where
    pair
    -}
