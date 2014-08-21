module Hamble.Audio (recorder, player) where

import Hamble.Constants 
import qualified Media.Streaming.GStreamer as Gst
import qualified System.Glib.MainLoop as G
import qualified System.Glib.GError as G

recorder = handlePipeline mkRecorder
player   = handlePipeline mkPlayer

handlePipeline builder server = do 
  (pipe, err) <- builder server
  case err of
    Just (G.GError _ _ msg) -> putStrLn msg
    Nothing -> case pipe of
                 Nothing    -> putStrLn "No voice pipe"
                 Just pipe' -> do
                   Gst.elementSetState pipe' Gst.StatePlaying
                   bus <- Gst.pipelineGetBus (Gst.castToPipeline pipe')

                   msg <- Gst.busTimedPop bus Nothing
                   return ()
                    
  
mkPlayer :: String -> IO (Maybe Gst.Element, Maybe G.GError)
mkPlayer serverAddr = Gst.parseLaunch $ 
  -- "udpsrc uri=udp://" ++ serverAddr ++ " port=" ++ show voicePort ++ " ! autoaudiosink"
  "audiotestsrc ! autoaudiosink"

mkRecorder :: String -> IO (Maybe Gst.Element, Maybe G.GError)
mkRecorder serverAddr = Gst.parseLaunch $ 
  "audiotestsrc ! udpsink host=" ++ serverAddr ++ " port=" ++ show voicePort

