{-# LANGUAGE RecordWildCards #-}

module Wire.Tutorial.Input ( Input(..)
                           , inputBehavior
                           ) where

import Control.Lens
import Data.Function (fix)
import Linear.V3

import Wire.Behavior
import Wire.Tutorial.Camera

-- | `Input` action taken from stdin
data Input
  = W -- ^ Move Forward
  | S -- ^ Move Backward
  | A -- ^ Move Left
  | D -- ^ Move Right
  | U -- ^ Move Up
  | J -- ^ Move Down
  | Q -- ^ Quit
  deriving (Show, Eq, Read)

inputBehavior :: (Monad m) => Input -> BehaviorU t m (Event t Camera) (Maybe Camera)
inputBehavior i = Behavior . const $ return . (inputProc i)
  where
    inputProc :: (Monad m) => Input -> Event t Camera -> Either () (Maybe Camera, BehaviorU t m (Event t Camera) (Maybe Camera))
    inputProc W = updCamera (cameraPosition . _z +~)
    inputProc S = updCamera (cameraPosition . _z -~)
    inputProc A = updCamera (cameraPosition . _x -~)
    inputProc D = updCamera (cameraPosition . _x +~)
    inputProc U = updCamera (cameraPosition . _y +~)
    inputProc J = updCamera (cameraPosition . _y -~)
    inputProc Q = return . Left $ ()
    -- inputProc _ = updCamera (const id)

    updCamera :: (Monad m) => (Float -> Camera -> Camera) -> Event t Camera -> Either () (Maybe Camera, BehaviorU t m (Event t Camera) (Maybe Camera))
    updCamera fn Event{..} = Right (Just cam, fix $ \b -> Behavior $ const $ const $ return $ Right (Just cam, b))
      where
        cam :: Camera
        cam = unEvent & snd & fn dv

