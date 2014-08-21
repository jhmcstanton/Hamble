{- Hamble client software for voice and text communication -}

module Main (main) where

import Hamble.Audio
import Hamble.Constants

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString 

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Control.Monad.Fix
import Control.Monad
import Control.Concurrent
import System.Environment
import qualified Media.Streaming.GStreamer as Gst

main :: IO ()
main = withSocketsDo $ do
  args <- getArgs
  case args of 
    []            -> putStrLn "Usage: hamble-client <server ip>"
    (serverArg:_) -> do
--      (vInfo: _) <- getAddrInfo Nothing (Just serverArg) (Just $ show voicePort)
      (tInfo: _) <- getAddrInfo Nothing (Just serverArg) (Just $ show textPort)
--      voiceSock  <- socket AF_INET Datagram 0
      textSock   <- socket AF_INET Stream   0 
  
      connect textSock (addrAddress tInfo)
--      connect voiceSock (addrAddress vInfo)

      forkOS $ handleVoice serverArg
      handleText textSock 
--      close voiceSock 
      close textSock
      putStrLn "Closing Hamble"


handleVoice :: String -> IO ()
handleVoice serverAddr= do
  Gst.init
  forkOS $ recorder serverAddr
  player serverAddr 
  return ()

handleText :: Socket -> IO ()
handleText sock = do
  reader  <- forkIO $ fix (\loop -> recv sock textPort >>= BC.putStrLn >> loop)
  fix (\loop -> do
    msg <- getLine
    case words msg of
      ("/quit":_) -> send sock (BC.pack msg) >> return ()
      _           -> send sock (BC.pack msg) >> loop)
  
  killThread reader
