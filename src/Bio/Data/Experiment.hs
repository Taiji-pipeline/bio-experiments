module Bio.Data.Experiment
    ( FileType(..)
    , File
    , location
    , tags
    , gzipped
    , emptyFile

    , FileSet(..)
    , _Single
    , _Paired

    , Replicate
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

emptyFile :: File filetype
emptyFile = File
    { fileLocation = ""
    , fileInfo = M.empty
    , fileTags = []
    , fileGzipped = False }

{-
class NGS experiment where
    pair
    -}
