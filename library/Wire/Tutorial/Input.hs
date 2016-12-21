{-# LANGUAGE RecordWildCards #-}

module Wire.Tutorial.Input where

-- module Wire.Tutorial.Input ( Input
--                            , inputBehavior
--                            , eventHandler
--                            , updateCamera
--                            ) where

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
  | Quit
  deriving (Show, Eq, Read)

inputBehavior :: Input -> Behavior t (Event t Camera) Camera
inputBehavior = Behavior . const . eventHandler

eventHandler :: Input -> Event t Camera -> (Camera, Behavior t (Event t Camera) Camera)
eventHandler W = updateCamera (cameraPosition . _z -~)
eventHandler S = updateCamera (cameraPosition . _z +~)
eventHandler A = updateCamera (cameraPosition . _x -~)
eventHandler D = updateCamera (cameraPosition . _x -~)
eventHandler U = updateCamera (cameraPosition . _y +~)
eventHandler J = updateCamera (cameraPosition . _y -~)
eventHandler _ = updateCamera (const id)

updateCamera :: (Float -> Camera -> Camera) -> Event t Camera -> (Camera, Behavior t (Event t Camera) Camera)
updateCamera fn Event{..} = (cam, fix $ \b -> Behavior $ const (\_ -> (cam, b)))
  where
    cam :: Camera
    cam = unEvent & snd & fn dv

-- updateState :: State -> Input -> Maybe State
-- updateState s W = Just $ s & camera . cameraPosition . _z -~ dv
-- updateState s S = Just $ s & camera . cameraPosition . _z +~ dv
-- updateState s A = Just $ s & camera . cameraPosition . _x -~ dv
-- updateState s D = Just $ s & camera . cameraPosition . _x +~ dv
-- updateState s U = Just $ s & camera . cameraPosition . _y -~ dv
-- updateState s J = Just $ s & camera . cameraPosition . _y +~ dv
-- updateState s _ = Nothing

-- cameraObj :: Behavior t [Input] Camera
-- cameraObj = Behavior (const inputHandler)
--   where
--     inputHandler :: [Input] -> Camera
--     inputHandler = undefined

-- idleing :: Behavior t ([Input], Camera) Camera
-- idleing = Behavior (const snd)

