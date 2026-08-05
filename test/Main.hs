module Main (main) where

import ClientSpec qualified
import RetrySpec qualified
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  ClientSpec.spec
  RetrySpec.spec
