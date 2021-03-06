{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Conduit.AesonSpec (spec) where

import Conduit
import Data.Aeson
import Data.Conduit.Aeson
import qualified Data.Map.Strict as Map
import Data.Scientific
import qualified Data.Text as T
import GHC.Exts
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

#if !MIN_VERSION_aeson(2,0,3)
instance Arbitrary Value where
  arbitrary = sized sizedArbitraryValue
#endif

sizedArbitraryValue :: Int -> Gen Value
sizedArbitraryValue n
  | n <= 0 = oneof [pure Null, bool, number, string]
  | otherwise = resize n' $ oneof [pure Null, bool, number, string, array, object']
  where
    n' = n `div` 2
    bool = Bool <$> arbitrary
    number = Number <$> (scientific <$> arbitrary <*> arbitrary)
    string = String <$> arbitrary
    array = Array . fromList <$> arbitrary
    object' = listToObject <$> arbitrary

listToObject :: [(String, Value)] -> Value
listToObject xs = object [(fromString k, v) | (k, v) <- xs]

spec :: Spec
spec = do
  describe "roundtrips" $ do
    prop "conduitArray" $ \(xs :: [Value]) -> do
      xs' <-
        runConduit
          (sourceLazy (encode (Array (fromList xs))) .| conduitArray .| sinkList)
      xs' `shouldBe` xs
    prop "conduitObject" $ \(ls :: [(String, Value)]) -> do
      let xs = Map.fromList ls
      xs' <-
        runConduit
          (sourceLazy (encode (listToObject (Map.toAscList xs))) .|
           conduitObject .|
           sinkList)
      Map.fromList xs' `shouldBe` xs
