import Test.Tasty
import Test.Tasty.HUnit
import Set1

main :: IO ()
main = defaultMain $
  testCase "Set 1 tests" $ do
    assertEqual "fiveRands with seed 1 test" (product fiveRands) 8681089573064486461641871805074254223660
    assertEqual "randString3 with seed 1 test" randString3 "lrf"

