module TestFileSystemSpec
  (
    spec
  ) where

import Control.Exception (evaluate)
import Data.List.NonEmpty (NonEmpty (..))
import Test.QuickCheck (Positive (..), property)
import Test.Tasty.Hspec (Spec, describe, errorCall, it, shouldBe, shouldThrow)
--import Main

spec :: Spec
spec = do
  --------------------------------- Task1 --------------------------------------

--  describe "main" $ do
--    it "check main" $ do
--      nextDay Monday `shouldBe` Tuesday

