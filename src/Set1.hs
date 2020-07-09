{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

fiveRands :: [Integer]
fiveRands = let
                (r1, s1) = rand $ mkSeed 1
                (r2, s2) = rand s1
                (r3, s3) = rand s2
                (r4, s4) = rand s3
                (r5, _)  = rand s4
            in [r1, r2, r3, r4, r5]


randLetter :: Seed -> (Char, Seed)
randLetter seed = let (val, seed') = rand seed
                  in (toLetter val, seed')

randString3 :: String
randString3 = let
                (c1, s1) = randLetter $ mkSeed 1
                (c2, s2) = randLetter s1
                (c3, _)  = randLetter s2
            in [c1, c2, c3]

