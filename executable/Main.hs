-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.

{-# LANGUAGE BangPatterns #-}

import Data.Function (fix, (&))
import System.IO (hReady, hGetChar, stdin)
import Text.Read (readMaybe)
import Data.Char (toUpper)

import Wire.Behavior
import Wire.Tutorial.Input
import Wire.Tutorial.Camera

main :: IO ()
main = do
  let camera = newCamera 0 0 0
  print camera
  runBehavior (Just camera) 0
  where
    runBehavior :: Maybe Camera -> Int -> IO ()
    runBehavior cam i = do
      cam' <- unBehavior (go cam) i []
      case cam' of
        Left _ -> return ()
        Right (cam'R, _) -> case cam'R of
          Nothing -> runBehavior cam (i+1)
          _ -> runBehavior cam'R (i+1)

    go :: Maybe Camera -> BehaviorU t IO [Input] (Maybe Camera)
    go c = case c of
      Nothing -> Behavior . const . const . return . Left $ ()
      _ -> liftBehavior (pollInputs 0 10) >>= go' c -- read 10 key presses at most

    go' :: Maybe Camera -> [Input] -> BehaviorU t IO [Input] (Maybe Camera)
    go' c [] = fix $ \b -> Behavior $ const $ const $ (c, b) & Right & return
    go' Nothing _ = Behavior . const . const . return . Left $ ()
    go' (Just c') (i:ixs) = Behavior $ \t ixs' -> do
      c'' <- unBehavior (inputBehavior i) t (Event (t, c'))
      case c'' of
        Left err -> err & Left & return
        Right (c'R, _) -> case c'R of
          Nothing -> () & Left & return
          Just c''R -> do
            print c''R
            unBehavior (go' (Just c''R) ixs) t ixs'

    pollInputs :: Int -> Int -> IO [Input]
    pollInputs !j !blockSz = do
      isStdinReady <- hReady stdin -- Todo : Enhance : non blocking polling, but likely to be a CPU hog as well.
      if (not isStdinReady || (j >= blockSz))
        then return []
        else do im <- hGetChar stdin >>= return . readMaybe . (:[]) . toUpper
                let
                  !nextJ = j+1
                  pollM = pollInputs nextJ blockSz
                case im of
                  Nothing -> pollM
                  Just !i -> do
                    !ixs <- pollM
                    return (i:ixs)

