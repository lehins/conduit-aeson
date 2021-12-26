{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Conduit.AesonSpec (spec) where

import Conduit
import Data.Aeson
import Data.Conduit.Aeson
import Data.GenValidity.Aeson ()
import Data.GenValidity.Map ()
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import GHC.Exts
import Test.Hspec
import Test.Validity


spec :: Spec
spec = do
  describe "roundtrips" $ do
    it "conduitArray" $ do
      forAllValid $ \(xs :: [Value]) -> do
        xs' <-
          runConduit
            (sourceLazy (encode (Array (fromList xs))) .| conduitArray .| sinkList)
        xs' `shouldBe` xs
    it "conduitObject" $ do
      forAllValid $ \(xs :: Map.Map T.Text Value) -> do
        xs' <-
          runConduit
            (sourceLazy (encode (Object (fromList (Map.toList xs)))) .| conduitObject .| sinkList)
        Map.fromList xs' `shouldBe` xs
