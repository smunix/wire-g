-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.
import Data.Function (fix, (&))
-- import System.IO

import Wire.Behavior
import Wire.Tutorial.Input
import Wire.Tutorial.Camera

main :: IO ()
main = runBehavior (Just $ newCamera 0 0 0) 0
  where
    runBehavior :: Maybe Camera -> Int -> IO ()
    runBehavior cam i = do
      cam' <- unBehavior (go cam) i []
      case cam' of
        Left _ -> return ()
        Right (cam'R, _) -> case cam'R of
          Nothing -> runBehavior cam (i+1)
          Just cam''R -> do
            print (i, cam''R)
            runBehavior cam'R (i+1)

    go :: Maybe Camera -> BehaviorU t IO [Input] (Maybe Camera)
    go c = case c of
      Nothing -> Behavior . const . const . return . Left $ ()
      _ -> liftBehavior pollInputs >>= go' c

    go' :: Maybe Camera -> [Input] -> BehaviorU t IO [Input] (Maybe Camera)
    go' c [] = fix $ \b -> Behavior $ const $ const $ (c, b) & Right & return
    go' Nothing _ = Behavior . const . const . return . Left $ ()
    go' (Just c') ixsA@(i:ixs) = Behavior $ \t ixs' -> do
      c'' <- unBehavior (inputBehavior i) t (Event (t, c'))
      case c'' of
        Left err -> err & Left & return
        Right (c'R, _) -> case c'R of
          Nothing -> () & Left & return
          Just c''R -> do
            print (c', ixsA)
            print (c''R, ixs')
            unBehavior (go' (Just c''R) ixs) t ixs'

    pollInputs :: IO [Input]
    pollInputs = fmap (concatMap (fmap fst . reads) . words) getLine

-- idle :: (Monad m) => BehaviorU t m ([Input], Camera) Camera
-- idle = undefined
