{-# LANGUAGE TemplateHaskell
, GeneralizedNewtypeDeriving
, RecordWildCards
#-}

module Wire.Behavior ( Behavior(..)
                     , Event(..)
                     , dv
                     ) where

import Prelude hiding ((.), id)
import Control.Arrow
import Control.Category
import Data.Function (fix)
import Data.Semigroup
import Data.Profunctor

-- | delta value to be applied on each input
dv :: Float
dv = 1

-- | A `Behavior` is a reactive relationship between input (a) and output (b)
--  >>> Made the `Behavior` definition to look like Automatons (see Wikipedia/Automaton)
newtype Behavior t a b = Behavior { unBehavior :: t -> a -> (b, Behavior t a b) }

-- | An `Event` is a value that appears to happen at a specific instant
newtype Event t a = Event { unEvent :: (t, a) } deriving (Functor)

instance Arrow (Behavior t) where
  arr f = fix $ \r -> Behavior $ const (\a -> (f a, r))
  first f = Behavior $ \t (a, b) -> let (a', fn) = unBehavior f t a in ((a', b), first fn)

instance Category (Behavior t) where
  id = arr id
  x . y = Behavior $ \t a ->
    let
      (yr, ybn) = unBehavior y t a
      (xr, xbn) = unBehavior x t yr
    in
      (xr, xbn . ybn)

instance (Semigroup b) => Semigroup (Behavior t a b) where
  a <> b = Behavior $ \t x ->
    let
      (ar, ab) = unBehavior a t x
      (br, bb) = unBehavior b t x
    in
      (ar <> br, ab <> bb)

instance Functor (Behavior t a) where
  fmap f a = Behavior $ \t i ->
    let
      (r, bn) = unBehavior a t i
    in
      (f r, fmap f bn)

instance Applicative (Behavior t a) where
  pure = arr . const
  f <*> x = Behavior $ \t a ->
    let
      (xr, xbn) = unBehavior x t a
      (fr, fbn) = unBehavior f t a
    in
      (fr xr, fbn <*> xbn)

instance Profunctor (Behavior t) where
  dimap l r x = Behavior $ \t a ->
    let
      (xr, xbn) = unBehavior x t (l a)
    in
      (r xr, dimap l r xbn)
