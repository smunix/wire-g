-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.
import Data.Function (fix, (&))

import Wire.Behavior
import Wire.Tutorial.Input
import Wire.Tutorial.Camera

main :: IO ()
main = runBehavior (newCamera 0 0 0) (0 :: Int)
  where
    runBehavior cam i = do
      cam' <- unBehavior (go cam) i []
      case cam' of
        Left _ -> return ()
        Right (cam'R, cam'Be) -> do
          print (i, cam'R)
          runBehavior cam'R (i+1)

    go :: Camera -> BehaviorU t IO [Input] Camera
    go c = liftBehavior pollInputs >>= go' c

    go' :: Camera -> [Input] -> BehaviorU t IO [Input] Camera
    go' c [] = fix $ \b -> Behavior $ const $ const $ (c, b) & Right & return
    go' c ixsA@(i:ixs) = Behavior $ \t ixs' -> do
      print (c, ixsA, ixs')
      c' <- unBehavior (inputBehavior i) t (Event (t, c))
      case c' of
        Left err -> err & Left & return
        Right (c'R, _) -> unBehavior (go' c'R ixs) t ixs'

    pollInputs :: IO [Input]
    pollInputs = fmap (concatMap (fmap fst . reads) . words) getLine

-- idleing :: (Monad m) => BehaviorU t m ([Input], Camera) Camera
-- idleing = undefined

-- cameraObj :: Behavior t [Input] Camera
-- cameraObj = Behavior (const inputHandler)
--   where
--     inputHandler :: [Input] -> Camera
--     inputHandler = undefined

-- idleing :: Behavior t ([Input], Camera) Camera
-- idleing = Behavior (const snd)
