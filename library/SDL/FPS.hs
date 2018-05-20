module SDL.FPS where

import qualified SDL
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (when)
import Data.Word (Word32)

class Monad m => FPS m where
  startFrame :: m Word32
  default startFrame :: MonadIO m => m Word32
  startFrame = liftIO SDL.ticks

  endFrame :: Int -> Word32 -> m ()
  default endFrame :: MonadIO m => Int -> Word32 -> m ()
  endFrame = endFrame'

instance FPS IO

-- | `endFrame`'s default definition
endFrame' :: MonadIO m => Int -> Word32 -> m ()
endFrame' fps startTicks = liftIO $ do
  endTicks <- SDL.ticks
  let diff = (endTicks - startTicks) * fps'
  when (msps > diff) $ do
    let ms = (msps - diff) `div` fps'
    SDL.delay (fromIntegral ms)
  where
    fps' = fromIntegral fps

-- | Same as default definition and prints fps and delay
endFrameDebug :: MonadIO m => Int -> Word32 -> m ()
endFrameDebug fps startTicks = liftIO $ do
  endTicks <- SDL.ticks
  let diff = (endTicks - startTicks) * fps'
  let ms = (msps - diff) `div` fps'
  when (msps > diff) $ SDL.delay (fromIntegral ms)
  putStrLn $ show fps ++ " fps - before delay " ++ show (endTicks - startTicks) ++ " ms" ++ " with expected max " ++ show (msps `div` fps') ++ " ms"
  where
    fps' = fromIntegral fps

-- | Milliseconds per second
msps :: Word32
msps = 1000