{-# LANGUAGE TemplateHaskell #-}

module Wire.Tutorial.Camera ( Camera(..)
                            , cameraPosition
                            , newCamera
                            ) where

import Control.Lens
import Linear.V3

-- | A basic definition of a `Camera` object along with its 3D space position
newtype Camera = Camera { _cameraPosition :: V3 Float } deriving (Eq, Show, Read)
makeLenses ''Camera

newCamera :: Float -> Float -> Float -> Camera
newCamera a b c = Camera $ V3 a b c
