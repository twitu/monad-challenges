{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import           MCPrelude
import           Set4

----------------------------------------------
-- Part 1
----------------------------------------------

fiveRands :: [Integer]
fiveRands = evalGen (sequence $ replicate 5 (Gen rand)) (mkSeed 1)

randLetter :: Gen Char
randLetter = do
  val <- Gen rand
  return . toLetter $ val

randString3 :: String
randString3 = evalGen (sequence $ replicate 3 randLetter) $ mkSeed 1

----------------------------------------------
-- Part 2
----------------------------------------------

randEven :: Gen Integer
randEven = do
  val <- Gen rand
  return $ val * 2

randOdd :: Gen Integer
randOdd = do
  val <- randEven
  return $ val + 1

randTen :: Gen Integer
randTen = do
  val <- Gen rand
  return $ val * 10

generalA :: (a -> b) -> Gen a -> Gen b
generalA op ga = ga >>= (return . op)

randEven' :: Gen Integer
randEven' = generalA (2 *) $ Gen rand

randOdd' :: Gen Integer
randOdd' = generalA (1 +) randEven'

randTen' :: Gen Integer
randTen' = generalA (10 *) $ Gen rand

----------------------------------------------
-- Part 3
----------------------------------------------

randPair :: Gen (Char, Integer)
randPair = randLetter >>= (\c -> (Gen rand) >>= (\i -> return (c, i)))

generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair ga gb = do
  a <- ga
  b <- gb
  return $ (a, b)

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB = liftM2

generalPair2 :: Gen a -> Gen b -> Gen (a, b)
generalPair2 = generalB (,)

----------------------------------------------
-- Part 4
----------------------------------------------

repRandom :: [Gen a] -> Gen [a]
repRandom = sequence

----------------------------------------------
-- Part 5
----------------------------------------------

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo = (>>=)

mkGen :: a -> Gen a
mkGen = return

----------------------------------------------
-- Part 6
----------------------------------------------

generalB2 :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB2 f ga gb = genTwo ga (\v1 -> genTwo gb (mkGen . f v1))

generalPair2' :: Gen a -> Gen b -> Gen (a, b)
generalPair2' = generalB2 (,)

repRandom' :: [Gen a] -> Gen [a]
repRandom' []   = mkGen []
repRandom' [ga] = generalA (: []) ga
repRandom' (ga : gas) =
  genTwo ga (\v -> genTwo (repRandom gas) (mkGen . (:) v))
