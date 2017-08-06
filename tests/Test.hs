{-# LANGUAGE DataKinds #-}
module Main where

import Data.Aeson
import Test.Tasty
import Bio.Data.Experiment
import qualified Data.Serialize as S
import           Test.Tasty.HUnit
import Data.Maybe

sortedBed :: File '[Sorted] 'Bed
sortedBed = emptyFile

sortedBam :: File '[Sorted, Pairend] 'Bam
sortedBam = emptyFile

filelist :: FileList '[ '[Sorted], '[Sorted, Pairend]] '[Bed, Bam]
filelist = FCons sortedBed $ FCons sortedBam $ FNil

testJSON :: Assertion
testJSON = assertBool "decode fail" $ filelist == fromJust (decode (encode filelist))

testSerialize :: Assertion
testSerialize = assertBool "decode fail" $ filelist ==
    (fromEither $ S.decode $ S.encode filelist)
  where
    fromEither (Left x) = error x
    fromEither (Right x) = x

main = defaultMain $ testGroup "Main"
    [ testCase "JSON" testJSON
    , testCase "Serialization" testSerialize ]
