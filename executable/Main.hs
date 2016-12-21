-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.
import Data.Function (fix, (&))

import Wire.Behavior
import Wire.Tutorial.Input
import Wire.Tutorial.Camera

main :: IO ()
main = runBehavior camera (0 :: Int)
  where
    runBehavior cam i = do
      cam' <- unBehavior cam i [A, A, W]
      case cam' of
        Left _ -> return ()
        Right (cam'R, cam'Be) -> do
          print (i, cam'R)
          if (i == 3) then return ()
            else runBehavior cam'Be (i+1)

camera :: BehaviorU t IO [Input] Camera
camera = go (newCamera 0 0 0)
  where
    go :: Camera -> BehaviorU t IO [Input] Camera
    go c = liftBehavior pollInputs >>= go' c

    go' :: Camera -> [Input] -> BehaviorU t IO [Input] Camera
    go' c [] = fix $ \b -> Behavior $ const $ const $ (c, b) & Right & return
    go' c (i:ixs) = Behavior $ \t ixs' -> do
      c' <- unBehavior (inputBehavior i) t (Event (t, c))
      case c' of
        Left err -> err & Left & return
        Right (c'R, _) -> unBehavior (go' c'R ixs) t ixs'

    pollInputs :: IO [Input]
    pollInputs = fmap processInput getLine

    processInput :: String -> [Input]
    processInput = concatMap (fmap fst . reads) . words

-- idleing :: (Monad m) => BehaviorU t m ([Input], Camera) Camera
-- idleing = undefined

-- cameraObj :: Behavior t [Input] Camera
-- cameraObj = Behavior (const inputHandler)
--   where
--     inputHandler :: [Input] -> Camera
--     inputHandler = undefined

-- idleing :: Behavior t ([Input], Camera) Camera
-- idleing = Behavior (const snd)
