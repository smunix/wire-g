{-# LANGUAGE TemplateHaskell #-}

module Wire.Tutorial.State () where

import Control.Lens
import Linear.V3

import Wire.Tutorial.Camera

-- | The `State` we will be updating in our application
data State = State { _camera :: Camera } deriving (Show)
makeLenses ''State
