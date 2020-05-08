module TestFileSystemSpec
  (
    spec
  ) where

--import Control.Exception (evaluate)
--import Data.List.NonEmpty (NonEmpty (..))
--import Test.QuickCheck (Positive (..), property)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe)
--import Main

spec :: Spec
spec = do
  --------------------------------- Task1 --------------------------------------

  describe "nothing" $ do
    it "checks nothing" $ do
      True `shouldBe` True

