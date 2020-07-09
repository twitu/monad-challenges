{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

----------------------------------------------
-- Part 1
----------------------------------------------

type Gen a = Seed -> (a, Seed)

fiveRands :: [Integer]
fiveRands = let
                (r1, s1) = rand $ mkSeed 1
                (r2, s2) = rand s1
                (r3, s3) = rand s2
                (r4, s4) = rand s3
                (r5, _)  = rand s4
            in [r1, r2, r3, r4, r5]


randLetter :: Gen Char
randLetter seed = let (val, seed') = rand seed
                  in (toLetter val, seed')

randString3 :: String
randString3 = let
                (c1, s1) = randLetter $ mkSeed 1
                (c2, s2) = randLetter s1
                (c3, _)  = randLetter s2
            in [c1, c2, c3]

----------------------------------------------
-- Part 2
----------------------------------------------

randEven :: Gen Integer
randEven seed = let (v, s') = rand seed
                in (v * 2, s')

randOdd :: Gen Integer
randOdd seed = let (v, s') = randEven seed
               in (v + 1, s')

randTen :: Gen Integer
randTen seed = let (v, s') = rand seed
               in (v * 10, s')

generalA :: (a -> b) -> Gen a -> Gen b
generalA op gen = \seed -> let (v , s') = gen seed
                           in (op v, s')

randEven' :: Gen Integer
randEven' seed = generalA (2*) rand seed

randOdd' :: Gen Integer
randOdd'  seed = generalA (1+) randEven' seed

randTen' :: Gen Integer
randTen'  seed = generalA (10*) rand seed

----------------------------------------------
-- Part 3
----------------------------------------------

randPair :: Gen (Char, Integer)
randPair seed = let (c, s')  = randLetter seed
                    (i, s'') = rand s'
                in ((c, i), s'')

generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair ga gb = \seed -> let (a', s')  = ga seed
                                 (b', s'') = gb s'
                             in ((a', b'), s'')

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB f ga gb = \seed -> let (a', s')  = ga seed
                                (b', s'') = gb s'
                             in (f a' b', s'')

generalPair2 :: Gen a -> Gen b -> Gen (a, b)
generalPair2 = generalB (,)

----------------------------------------------
-- Part 4
----------------------------------------------

repRandom :: [Gen a] -> Gen [a]
repRandom [] = \seed -> ([], seed)
repRandom [ga] = generalA (:[]) ga
repRandom (ga:gas) = generalB (:) ga (repRandom gas)

----------------------------------------------
-- Part 5
----------------------------------------------

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo ga f = uncurry f . ga

mkGen :: a -> Gen a
mkGen a = \seed -> (a, seed)

