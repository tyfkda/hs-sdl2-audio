{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

import Data.IORef
import Control.Monad
import Control.Concurrent
import Data.Int (Int16, Int32)
import SDL
import qualified Data.Vector.Storable.Mutable as V

sinSamples :: [Int16]
sinSamples =
  map (\n ->
         let t = fromIntegral n * 2 * pi / 48000 :: Double
             freq = 440
         in round (fromIntegral (maxBound `div` 2 :: Int16) * sin (t * freq)))
      [0 :: Int32 ..]

audioCB :: IORef [Int16] -> AudioFormat sampleType -> V.IOVector sampleType -> IO ()
audioCB samples format buffer =
  case format of
    Signed16BitLEAudio ->
      do samples' <- readIORef samples
         let n = V.length buffer
         zipWithM_ (V.write buffer)
                   [0 ..]
                   (take n samples')
         writeIORef samples
                    (drop n samples')
    _ -> error "Unsupported audio format"

main :: IO ()
main = do
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer

  samples <- newIORef sinSamples
  (device,spec) <-
    openAudioDevice
      OpenDeviceSpec {SDL.openDeviceFreq =
                        Mandate 48000
                     ,SDL.openDeviceFormat =
                        Mandate Signed16BitNativeAudio
                     ,SDL.openDeviceChannels =
                        Mandate Mono
                     ,SDL.openDeviceSamples = 4096 * 2
                     ,SDL.openDeviceCallback = audioCB samples
                     ,SDL.openDeviceUsage = ForPlayback
                     ,SDL.openDeviceName = Nothing}
  setAudioDevicePlaybackState device Play
  -- forever (threadDelay maxBound)

  appLoop renderer (device, spec, samples)

appLoop :: Renderer -> (AudioDevice, AudioSpec, IORef [Int16]) -> IO ()
appLoop renderer audioInfo = do
  --threadDelay 1000000  - (1)
  events <- pollEvents
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events
  rendererDrawColor renderer $= V4 0 0 255 255
  clear renderer
  present renderer
  unless qPressed (appLoop renderer audioInfo)
