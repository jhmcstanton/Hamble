{- Audio and chat backend, with many ideas borrowed from haskelwiki's
  "Implement a Chat Server" 
-} 

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Data.ByteString
import qualified Data.ByteString.Char8 as BC
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad.Fix
import Control.Monad
import Control.Exception(try)
--module Main (main) where


maxClients  = 20
voicePort   = 9900 
textPort    = 9901  

type Msg =  (Int, ByteString)

main :: IO ()
main = withSocketsDo $ do
  voiceChan <- newChan
  textChan  <- newChan 
  ids       <- newChan -- pool of ids available to chat users
  writeList2Chan ids [1..maxClients]
  
  voiceSock <- socket AF_INET Datagram 0
  textSock  <- socket AF_INET Stream   0 

  setSocketOption voiceSock ReuseAddr 1
  setSocketOption textSock ReuseAddr 1

  bindSocket voiceSock (SockAddrInet (fromIntegral voicePort) iNADDR_ANY)
  bindSocket textSock  (SockAddrInet (fromIntegral textPort) iNADDR_ANY)

  listen voiceSock maxClients
  listen textSock maxClients
  
  listenLoop textSock voiceSock textChan voiceChan ids
    
listenLoop :: Socket -> Socket -> Chan Msg -> Chan Msg -> Chan Int -> IO ()
listenLoop textSock voiceSock textChan voiceChan ids = do
  (textClient, _)   <- accept textSock
  (voiceClient, _)  <- accept voiceSock -- this may not be the right way to do this
  -- might be better to send an id to "authenticate" that both requests are the same ^^       
  clientID <- readChan ids
  forkFinally (handleConn textChan voiceChan textClient voiceClient clientID) (\_ -> writeChan ids clientID)
  listenLoop textSock voiceSock textChan voiceChan ids

handleConn :: Chan Msg -> Chan Msg -> Socket -> Socket -> Int -> IO ()
handleConn textChan voiceChan tClient vClient id = do
  let sendText msg  = writeChan textChan (id, msg)
  let sendVoice msg = writeChan voiceChan (id, msg)  
  listenTChan       <- dupChan textChan
  listenVChan       <- dupChan voiceChan
        
  send tClient $ BC.pack "Welcome! Who are you?"
  name <- recv tClient textPort
  sendText $ newUserMsg name
      
  txtReader <- forkIO $ fix (\loop -> chanListener id listenTChan tClient >> loop)
  vListener <- forkIO $ fix (\loop -> chanListener id listenVChan vClient >> loop)
    
  vWriter   <- forkIO $ fix $ \loop -> do
    msg <- recv vClient voicePort
    sendVoice msg
    loop
           
  txtWriter <- forkIO $ fix $ \loop -> do
    msg <- recv tClient textPort
    case words (BC.unpack msg) of
      ("/quit":_) -> return ()           
      _           -> sendText msg >> loop

  killThread vWriter
  close vClient
  close tClient

newUserMsg :: ByteString -> ByteString
newUserMsg name = BC.pack $ "( ^(0 0)^ )" ++ BC.unpack name ++ " is in the pen."

chanListener :: Int -> Chan Msg -> Socket -> IO () 
chanListener clientID chan client = do 
  (id', msg) <- readChan chan
  when (id' /= clientID) $ send client msg >> return ()
