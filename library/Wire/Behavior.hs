{-# LANGUAGE TemplateHaskell
, GeneralizedNewtypeDeriving
, RecordWildCards
#-}

module Wire.Behavior ( Behavior(..)
                     , BehaviorU
                     , Event(..)
                     , liftBehavior
                     , dv
                     ) where

import Prelude hiding ((.), id)
import Control.Arrow
import Control.Category
import Data.Function (fix, (&))
import Data.Semigroup
import Data.Profunctor

-- | delta value to be applied on each input
dv :: Float
dv = 1

-- | A `Behavior` is a reactive relationship between input (a) and output (b)
--  >>> Made the `Behavior` definition to look like Automatons (see Wikipedia/Automaton)
newtype Behavior t e m a b = Behavior { unBehavior :: t -> a -> m (Either e (b, Behavior t e m a b)) }
type BehaviorU t m = Behavior t () m

-- | An `Event` is a value that appears to happen at a specific instant
newtype Event t a = Event { unEvent :: (t, a) } deriving (Functor)

instance (Monad m) => Arrow (Behavior t e m) where
  arr f = fix $ \r -> Behavior $ const (\a -> (f a, r) & Right & return)
  first f = Behavior $ \t (a, b) -> do
    a' <- unBehavior f t a
    return $ case a' of
      Left err -> Left err
      Right (a'R, fnBe) -> Right ((a'R, b), first fnBe)

instance (Monad m) => Category (Behavior t e m) where
  id = arr id
  g . f = Behavior $ \t a -> do
    f' <- unBehavior f t a
    case f' of
      Left err -> return $ Left err
      Right (f'R, f'Be) -> do
        g' <- unBehavior g t f'R
        case g' of
          Left err -> return $ Left err
          Right (g'R, g'Be) -> return $ Right (g'R, g'Be . f'Be)

instance (Monad m, Semigroup b) => Semigroup (Behavior t e m a b) where
  a <> b = Behavior $ \t i -> do
    a' <- unBehavior a t i
    b' <- unBehavior b t i
    case (a', b') of
      (Left a'err, _) -> return $ Left a'err
      (_, Left b'err) -> return $ Left b'err
      (Right (a'R, a'Be), Right (b'R, b'Be)) -> return $ Right (a'R <> b'R, a'Be <> b'Be)

instance (Monad m) => Functor (Behavior t e m a) where
  fmap f a = Behavior $ \t i -> do
    a' <- unBehavior a t i
    case a' of
      Left err -> return $ Left err
      Right (a'R, a'Be) -> return $ Right (f a'R, fmap f a'Be)

instance (Monad m) => Applicative (Behavior t e m a) where
  pure = arr . const
  f <*> a = Behavior $ \t i -> do
    f' <- unBehavior f t i
    a' <- unBehavior a t i
    case (f', a') of
      (Left f'err, _) -> return $ Left f'err
      (_, Left a'err) -> return $ Left a'err
      (Right (f'R, f'Be), Right (a'R, a'Be)) -> return $ Right (f'R a'R, f'Be <*> a'Be)

instance (Monad m) => Monad (Behavior t e m a) where
  return = pure
  m >>= f = Behavior $ \t i -> do
    m' <- unBehavior m t i
    case m' of
      Left err -> err & Left & return
      Right (m'R, _) -> do
        f' <- unBehavior (f m'R) t i
        case f' of
          Left err -> err & Left & return
          Right (f'R, f'Be) -> (f'R, f'Be) & Right & return

instance (Monad m) => Profunctor (Behavior t e m) where
  dimap l r a = Behavior $ \t i -> do
    a' <- unBehavior a t (l i)
    case a' of
      Left a'err -> return $ Left a'err
      Right (a'R, a'Be) -> return $ Right (r a'R, dimap l r a'Be)

-- instance (Monad m, MonadIO m) => MonadIO (Behavior t e m a) where
--   liftIO = liftBehavior . liftIO

liftBehavior :: (Monad m) => m b -> Behavior t e m a b
liftBehavior m = fix $ \b -> Behavior $ const $ const $ do
  m' <- m
  (m', b) & Right & return
