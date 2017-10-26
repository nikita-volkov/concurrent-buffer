module Main where

import Prelude
import Bug
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck.Instances
import qualified ConcurrentBuffer as A
import qualified Data.ByteString as D
import qualified Data.ByteString.Internal as B


main =
  defaultMain $
  testGroup "All tests"
  [
    testCase "Interleaving" $ do
      let (input1, input2, input3) = (1, 2, 3) :: (Word64, Word64, Word64)
      buffer <- A.new 2
      A.pushStorable buffer input1
      A.pushStorable buffer input2
      output1 <- A.pullStorable buffer
      A.pushStorable buffer input3
      output2 <- A.pullStorable buffer
      output3 <- A.pullStorable buffer
      assertEqual (show output1) input1 output1
      assertEqual (show output2) input2 output2
      assertEqual (show output3) input3 output3
    ,
    testProperty "Numbers" $ \(inputs :: [Word64]) ->
      unsafePerformIO $ do
        buffer <- A.new 2
        forM_ inputs $ \input -> A.pushStorable buffer input
        outputs <- (traverse (const (A.pullStorable buffer)) inputs)
        return (inputs === outputs)
    ,
    testProperty "Concatenation of pushed bytestrings equals the binary representation" $ \(inputs :: [ByteString]) ->
      unsafePerformIO $ do
        buffer <- A.new (2 ^ 8)
        forM_ inputs $ \input -> A.pushBytes buffer input
        let concattedInputs = mconcat inputs
        output <- A.pullBytes buffer (D.length concattedInputs)
        return (concattedInputs === output)
    ,
    testProperty "Concurrent push and pull equality" $ \(inputs :: Vector ByteString) ->
      unsafePerformIO $ do
        buffer <- A.new (2 ^ 8)
        let !amounts = D.length <$!> inputs
        forkIO $ forM_ inputs $ \input -> A.pushBytes buffer input
        outputs <- traverse (A.pullBytes buffer) amounts
        return (inputs == outputs)
  ]

inspect :: A.Buffer -> IO ()
inspect buffer =
  print . D.unpack =<< A.getBytes buffer
