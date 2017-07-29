module Bio.Data.Experiment
    ( FileType(..)
    , File
    , location
    , tags
    , emptyFile

    , MaybeTagged
    , untagMaybe
    , GZipped
    , Pairend
    , Sorted
    , MaybePaired
    , isPaired

    , Replicate(..)
    , files
    , info
    , number
    , Experiment(..)

    , ATACSeq
    , RNASeq
    ) where

import qualified Data.Map.Strict as M

import Bio.Data.Experiment.RNASeq
import Bio.Data.Experiment.ATACSeq
import Bio.Data.Experiment.File
import Bio.Data.Experiment.Types
import Bio.Data.Experiment.Replicate

data Sorted
data Pairend
data GZipped

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
