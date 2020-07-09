import Test.Tasty
import Test.Tasty.HUnit
import Set1
import MCPrelude

main :: IO ()
main = defaultMain $
  testCase "Set 1 tests" $ do
    assertEqual "fiveRands with seed 1 test" (product fiveRands) 8681089573064486461641871805074254223660
    assertEqual "randString3 with seed 1 test" randString3 "lrf"
    assertEqual "rand Even Odd and Ten with seed 1 test" (let (v1, _) = randEven' $ mkSeed 1
                                                              (v2, _) = randOdd'  $ mkSeed 1
                                                              (v3, _) = randTen'  $ mkSeed 1
                                                          in v1 * v2 * v3) 189908109902700

