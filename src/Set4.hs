{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set4 where

import           MCPrelude

--------------------------------------------------------
-- Part 1
--------------------------------------------------------

-- type Gen a = Seed -> (a, Seed)

-- data Maybe a = Just a | Nothing

-- genTwo :: Gen a -> (a -> Gen b) -> Gen b
-- generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c

-- link :: Maybe a -> (a -> Maybe b) -> Maybe b
-- yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c

-- common abstraction
-- link :: m a -> (a -> m b) -> m b
-- ylink :: (a -> b -> c) -> m a -> m b -> m c

--------------------------------------------------------
-- Part 2
--------------------------------------------------------

-- Implement `generalB2` in `Set1.hs`

--------------------------------------------------------
-- Part 3
--------------------------------------------------------

-- Implement `repRandom'` in `Set1.hs`

--------------------------------------------------------
-- Part 4
--------------------------------------------------------

-- abstract `link` into a common function
class Monad m where
  (>>=) :: m a -> (a -> m b) -> m b  -- idiomatic symbol for bind function
  return :: a -> m a

data Maybe a = Just a | Nothing

instance Monad Maybe where
  return = Just

  Just val >>= f = f val
  Nothing  >>= _ = Nothing

instance Show a => Show (Maybe a) where
  show (Just a) = "Just " ++ show a
  show Nothing  = "Nothing"

instance Eq a => Eq (Maybe a) where
  (==) (Just a) (Just b) = a == b
  (==) Nothing  Nothing  = True
  (==) _        _        = False

instance Monad [] where
  return = flip (:) []

  []       >>= _ = []
  [a     ] >>= f = f a
  (a : as) >>= f = f a ++ (as >>= f)

newtype Gen a = Gen { runGen :: Seed -> (a, Seed) }

evalGen :: Gen a -> Seed -> a
evalGen ga s = fst . (runGen ga) $ s

instance Monad Gen where
  return a = Gen (\seed -> (a, seed))

  ga >>= f2 = Gen (\seed -> let (v, s') = runGen ga seed in runGen (f2 v) s')

--------------------------------------------------------
-- Part 5
--------------------------------------------------------

-- from Set1.hs
sequence :: Monad m => [m a] -> m [a]
sequence []         = return []
sequence (ga : gas) = ga >>= (\a -> sequence gas >>= (\as -> return $ a : as))

liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f ma mb = ma >>= (\a -> mb >>= (return . f a))

-- from Set2.hs

join :: Monad m => m (m a) -> m a
join = flip (>>=) id

(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) = flip (>>=)

-- from Set3.hs

liftM3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM3 f ma mb mc = ma >>= (\a -> mb >>= (\b -> mc >>= (return . f a b)))

ap :: Monad m => m (a -> b) -> m a -> m b
ap mf ma = mf >>= (\f -> ma >>= (return . f))
