{-# LANGUAGE TemplateHaskell #-}

module Wire.Tutorial.Camera ( Camera
                       , cameraPosition
                       ) where

import Control.Lens
import Linear.V3

-- | A basic definition of a `Camera` object along with its 3D space position
newtype Camera = Camera { _cameraPosition :: V3 Float } deriving (Eq, Show, Read)
makeLenses ''Camera
