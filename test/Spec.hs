import Control.Monad (forM_)
import Data.Either (isLeft, isRight)
import Parsers (parse)
import Test.Hspec

main :: IO ()
main = do
  success <- readFile "./test/case1.txt"
  failure <- readFile "./test/case2.txt"
  hspec $ do
    describe "parse ok" $ do
      forM_ (lines success) $ \case' -> do
        it (case' ++ " should parse") $ do
          parse case' `shouldSatisfy` isRight
    describe "parse error" $ do
      forM_ (lines failure) $ \case' -> do
        it (case' ++ " should fail") $ do
          parse case' `shouldSatisfy` isLeft
