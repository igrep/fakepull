{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Test.Hspec
import           Test.Hspec.QuickCheck

import           Test.Pull.Fake.Pure


main :: IO ()
main = hspec $
  describe "Test.Pull.Fake.Pure" $
    describe "pull" $
      prop "pulls from the stack" $ \(payloads :: [Int]) -> do
        let action =
              pull >>= \case
                Nothing -> return []
                Just payload -> (payload :) <$> action
            (pulledThings, leftPayloads) = runPullM action payloads
        pulledThings `shouldBe` payloads
        leftPayloads `shouldBe` []
