{- Audio and chat backend, with many ideas borrowed from haskelwiki's
  "Implement a Chat Server" 
-} 

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString 
import Data.ByteString hiding (putStrLn)
import qualified Data.ByteString.Char8 as BC
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad.Fix
import Control.Monad
--module Main (main) where


maxClients  = 20
maxRecv     = 4092
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
  putStrLn "Sockets created" 

  setSocketOption voiceSock ReuseAddr 1
  setSocketOption textSock ReuseAddr 1
  putStrLn "Socket options set"

  bindSocket voiceSock (SockAddrInet (fromIntegral voicePort) iNADDR_ANY)
  bindSocket textSock  (SockAddrInet (fromIntegral textPort) iNADDR_ANY)
  putStrLn "Sockets bound"

--  listen voiceSock maxClients
  listen textSock maxClients
  putStrLn "Listening"
  
  forkIO $ listenLoop textSock voiceSock textChan voiceChan ids
  
  fix (\loop -> readChan textChan >>= BC.putStrLn . snd >> loop)
    
listenLoop :: Socket -> Socket -> Chan Msg -> Chan Msg -> Chan Int -> IO ()
listenLoop textSock voiceSock textChan voiceChan ids = do
  (textClient, _)   <- accept textSock
--  (voiceClient, _)  <- accept voiceSock -- this may not be the right way to do this
  -- might be better to send an id to "authenticate" that both requests are the same ^^       
  clientID <- readChan ids
  putStrLn $ "Connected client " ++ show clientID ++ " to text and voice"
  forkFinally (handleConn textChan voiceChan textClient voiceSock clientID) (\_ -> writeChan ids clientID)
  listenLoop textSock voiceSock textChan voiceChan ids

handleConn :: Chan Msg -> Chan Msg -> Socket -> Socket -> Int -> IO ()
handleConn textChan voiceChan tClient vClient id = do
  let sendText msg  = writeChan textChan (id, msg)
  let sendVoice msg = writeChan voiceChan (id, msg)  
  listenTChan       <- dupChan textChan
  listenVChan       <- dupChan voiceChan
        
  send tClient $ BC.pack "Welcome! Who are you?"
  name <- recv tClient maxRecv
  let msgPrefix = BC.pack $ BC.unpack name ++ ":"

  sendText $ newUserMsg name
      
  txtReader <- forkIO $ fix (\loop -> chanListener id listenTChan tClient >> loop)
  vListener <- forkIO $ fix (\loop -> chanListener id listenVChan vClient >> loop)
  putStrLn "passed readers"
  threadDelay 2000
    
  vWriter   <- forkIO $ fix $ \loop -> do
    msg <- recv vClient maxRecv
    sendVoice msg
    loop
  
  fix $ \loop -> do
    msg <- recv tClient maxRecv
    case words (BC.unpack msg) of
      ("/quit":_) -> return ()           
      _           -> sendText (append msgPrefix msg) >> loop
  
  killThread vWriter
  killThread txtReader
  killThread vListener
--  close vClient
  close tClient

newUserMsg :: ByteString -> ByteString
newUserMsg name = BC.pack $ "( ^(0 0)^ ) " ++ BC.unpack name ++ " is in the pen."

chanListener :: Int -> Chan Msg -> Socket -> IO () 
chanListener clientID chan client = do 
  (id', msg) <- readChan chan
  when (id' /= clientID) $ send client msg >> return ()
