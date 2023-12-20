module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Control.Monad (forM_)

import MyLib

main :: IO ()
main = defaultMain allTests

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ testCase "abcd < ab" $
      zipLong [1, 2, 3, 4] "ab" @?= [(1, 'a'), (2, 'b'), (3, 'a'), (4, 'b')]
  , testCase "zipLong empty first list" $
      zipLong [] "sfd" @?= ([] :: [(Int, Char)])
  , testCase " 'abc' equals 'abc' " $
      zipLong [1, 2, 3] "abc" @?= [(1, 'a'), (2, 'b'), (3, 'c')]
  , testCase "ab < abcd" $
      zipLong [1, 2] "abcd" @?= [(1, 'a'), (2, 'b'), (1, 'c'), (2, 'd')]
  ]

genList :: Gen [Int]
genList = Gen.list (Range.linear 0 100) Gen.enumBounded

-- Длина результата zipLong не меньше максимальной длины входных списков
prop_zipLongLength :: Property
prop_zipLongLength = property $ do
  as <- forAll genList
  bs <- forAll genList
  let result = zipLong as bs
  if length as == 0 || length bs == 0
    then
      assert $ length result == 0
    else
      assert $ length result >= max (length as) (length bs)

allTests :: TestTree
allTests = testGroup "All Tests"
  [ unitTests
  , testProperty "zipLong property" prop_zipLongLength
  ]