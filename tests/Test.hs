{-# LANGUAGE DataKinds #-}
module Main where

import Data.Aeson
import Test.Tasty
import Bio.Data.Experiment
import qualified Data.Serialize as S
import           Test.Tasty.HUnit
import Data.Maybe
import Data.Singletons
import Data.Singletons.Prelude
import Data.Singletons.Prelude.List

{-
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
-}

testTypeFun :: Assertion
testTypeFun = assertBool "type" $ and dat
  where

    dat = [ (Proxy :: Proxy (Elem 'Gzip '[Gzip])) == true
          , (Proxy :: Proxy (Elem 'Gzip '[Gzip, NameSorted])) == true
          , (Proxy :: Proxy (Elem 'Gzip '[NameSorted, PairedEnd])) == false
          , (Proxy :: Proxy (Delete 'Gzip '[Gzip, NameSorted])) ==
            (Proxy :: Proxy '[NameSorted])
          , (Proxy :: Proxy (Elem 'Gzip (Insert 'Gzip '[NameSorted]))) == true
          ]
    true = Proxy :: Proxy 'True
    false = Proxy :: Proxy 'False

main = defaultMain $ testGroup "Main"
    [ -- testCase "JSON" testJSON
    --, testCase "Serialization" testSerialize
     testCase "Type function" testTypeFun
    ]
