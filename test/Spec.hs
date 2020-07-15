import Test.Tasty
import Test.Tasty.HUnit
import MCPrelude
import Prelude (IO)
import Set1
import Set2

main :: IO ()
main = defaultMain testCases

testCases :: TestTree
testCases = testGroup "Tests" [set1Test, set2Test]

set1Test = testCase "Set 1 tests" $ do
  assertEqual "fiveRands with seed 1 test" (product fiveRands) 8681089573064486461641871805074254223660
  assertEqual "randString3 with seed 1 test" randString3 "lrf"
  assertEqual "rand Even Odd and Ten with seed 1 test" (let (v1, _) = randEven' $ mkSeed 1
                                                            (v2, _) = randOdd'  $ mkSeed 1
                                                            (v3, _) = randTen'  $ mkSeed 1
                                                      in v1 * v2 * v3) 189908109902700
  assertEqual "randPair with seed 1 test" (let (p, _) = randPair $ mkSeed 1 in p) ('l', 282475249)
  assertEqual "generalPair and randPair comparison test" (randPair $ mkSeed 1) (generalPair randLetter rand $ mkSeed 1)
  assertEqual "generalPair 2 and generalPair comparison test" (generalPair randLetter rand $ mkSeed 1) (generalPair2 randLetter rand $ mkSeed 1)
  assertEqual "repRandom and 3 randLetter comparison test" (let (v, _) = repRandom (replicate 3 randLetter) (mkSeed 1)
                                                              in v) randString3

set2Test = testCase "Set 2 tests" $ do
  assertEqual "Set 2" (queryGreek greekDataA "alpha") (Just 2.0)
  assertEqual "Set 2" (queryGreek greekDataA "beta") Nothing
  assertEqual "Set 2" (queryGreek greekDataA "gamma") (Just 3.3333333333333335)
  assertEqual "Set 2" (queryGreek greekDataA "delta") Nothing
  assertEqual "Set 2" (queryGreek greekDataA "zeta") Nothing
  assertEqual "Set 2" (queryGreek greekDataB "rho") Nothing
  assertEqual "Set 2" (queryGreek greekDataB "phi") (Just 0.24528301886792453)
  assertEqual "Set 2" (queryGreek greekDataB "chi") (Just 9.095238095238095)
  assertEqual "Set 2" (queryGreek greekDataB "psi") Nothing
  assertEqual "Set 2" (queryGreek greekDataB "omega") (Just 24.0)

  assertEqual "Set 2" (queryGreek2 greekDataA "alpha") (Just 2.0)
  assertEqual "Set 2" (queryGreek2 greekDataA "beta") Nothing
  assertEqual "Set 2" (queryGreek2 greekDataA "gamma") (Just 3.3333333333333335)
  assertEqual "Set 2" (queryGreek2 greekDataA "delta") Nothing
  assertEqual "Set 2" (queryGreek2 greekDataA "zeta") Nothing
  assertEqual "Set 2" (queryGreek2 greekDataB "rho") Nothing
  assertEqual "Set 2" (queryGreek2 greekDataB "phi") (Just 0.24528301886792453)
  assertEqual "Set 2" (queryGreek2 greekDataB "chi") (Just 9.095238095238095)
  assertEqual "Set 2" (queryGreek2 greekDataB "psi") Nothing
  assertEqual "Set 2" (queryGreek2 greekDataB "omega") (Just 24.0)

  assertEqual "yLink" (addSalaries salaries "bob" "alice") (addSalaries2 salaries "bob" "alice")
  assertEqual "yLink" (addSalaries salaries "bob" "alic") (addSalaries2 salaries "bob" "alic")

  assertEqual "tailProd" (tailProd []) Nothing
  assertEqual "tailProd" (tailProd [1, 2, 3]) (Just 6)
  assertEqual "tailProd" (tailProd [2]) (Just 1)

  assertEqual "tailSum" (tailSum []) Nothing
  assertEqual "tailSum" (tailSum [1, 2, 3]) (Just 5)
  assertEqual "tailSum" (tailSum [2]) (Just 0)

  assertEqual "tailMax" (tailMax [1]) Nothing
  assertEqual "tailMax" (tailMax [1, 5, 4]) (Just 5)

  assertEqual "tailMin" (tailMin [2]) Nothing
  assertEqual "tailMin" (tailMin [1, 5, 4]) (Just 4)

