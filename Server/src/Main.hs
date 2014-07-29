{- Audio and chat backend, with many ideas borrowed from haskelwiki's
  "Implement a Chat Server" 
-} 

module Main (main) where

import Hamble.Utils
import Hamble.Constants
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import qualified Data.ByteString.Char8 as BC
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad.Fix

main :: IO ()
main = withSocketsDo $ do
  voiceChan <- newChan
  textChan  <- newChan 
  ids       <- newChan -- pool of ids available to chat users, server side only
  aliases   <- newMVar [] 
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
  
  forkIO $ listenLoop textSock voiceSock textChan voiceChan ids aliases
  
  fix (\loop -> readChan textChan >>= BC.putStrLn . snd >> loop)
    
