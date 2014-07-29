{-
  Functions used by Main to run server
-}

module Hamble.Utils where

import Hamble.Constants
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString 
import Data.ByteString hiding (putStrLn)
import qualified Data.ByteString.Char8 as BC
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad.Fix
import Control.Monad
import Data.List(delete)

type Msg =  (Int, ByteString)

listenLoop :: Socket -> Socket -> Chan Msg -> Chan Msg -> Chan Int -> MVar [ByteString] -> IO ()
listenLoop textSock voiceSock textChan voiceChan ids aliases = do
  (textClient, _)   <- accept textSock
--  (voiceClient, _)  <- accept voiceSock -- this may not be the right way to do this
  -- might be better to send an id to "authenticate" that both requests are the same ^^       
  clientID <- readChan ids
  putStrLn $ "Connected client " ++ show clientID ++ " to text and voice"
  forkFinally (handleConn textChan voiceChan textClient voiceSock clientID aliases) (\_ -> writeChan ids clientID)
  listenLoop textSock voiceSock textChan voiceChan ids aliases

handleConn :: Chan Msg -> Chan Msg -> Socket -> Socket -> Int -> MVar [ByteString] -> IO ()
handleConn textChan voiceChan tClient vClient id aliases = do
  let sendText msg  = writeChan textChan (id, msg)
  let sendVoice msg = writeChan voiceChan (id, msg)  
  listenTChan       <- dupChan textChan
  listenVChan       <- dupChan voiceChan
        
  send tClient $ BC.pack "Welcome! Who are you?"
  name <- recv tClient maxRecv
  let msgPrefix = BC.pack $ BC.unpack name ++ ":> "

  sendText $ newUserMsg name
  group <- takeMVar aliases
  send tClient $ BC.pack "In the pen: "
  send tClient $ BC.concat group
  putMVar aliases (name : group)
     
  txtReader <- forkIO $ fix (\loop -> chanListener id listenTChan tClient >> loop)
  vListener <- forkIO $ fix (\loop -> chanListener id listenVChan vClient >> loop)
    
  vWriter   <- forkIO $ fix $ \loop -> do
    msg <- recv vClient maxRecv
    sendVoice msg
    loop
  
  fix $ \loop -> do
    msg <- recv tClient maxRecv
    case words (BC.unpack msg) of
      ("/quit":_) -> return ()
      ("/list":_) -> do
                     group <- takeMVar aliases 
                     send tClient $ BC.intercalate (BC.pack "\n") group
                     putMVar aliases group
                     loop           
      _           -> sendText (append msgPrefix msg) >> loop
  
  sendText $ BC.concat [name, BC.pack " has left the pen"]
  group <- takeMVar aliases
  putMVar aliases $ delete name group
  killThread vWriter
  killThread txtReader
  killThread vListener
  close tClient

newUserMsg :: ByteString -> ByteString
newUserMsg name = BC.pack $ "( ^(0 0)^ ) " ++ BC.unpack name ++ " is in the pen."

chanListener :: Int -> Chan Msg -> Socket -> IO () 
chanListener clientID chan client = do 
  (id', msg) <- readChan chan
  when (id' /= clientID) $ send client msg >> return ()
