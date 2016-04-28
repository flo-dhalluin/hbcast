{-# LANGUAGE OverloadedStrings #-}

module Lib.Server (runServer) where

import Data.Conduit
import Data.Conduit.Network
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO)-- forkIO .. 
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM (atomically)

import qualified Data.ByteString.Char8 as BC
import Data.Char (toUpper)
import Lib.Message

data Message = Plus Double
             | Minus Double deriving (Show)

updateVal :: Double -> Message -> Double
updateVal x (Plus d) = x + d
updateVal x (Minus d) = x - d

commanderThread :: Double -> TChan Message -> IO ()
commanderThread val chan = do
  msg <- atomically $ readTChan chan
  putStrLn $ show msg
  putStrLn $ show val
  commanderThread (updateVal val msg) chan
  
-- upercaseCdt = do 
--   mb <- await
--   case mb of 
--     Nothing -> return ()
--     Just b -> yield $ BC.map toUpper b

parseMsg :: BC.ByteString -> Maybe Message
parseMsg str = let 
  sign = BC.head str
  val = (read $ BC.unpack $ BC.tail $ str) :: Double
  in
    case sign of 
      '+' -> Just (Plus val)
      '-' -> Just (Minus val)
      otherwise -> Nothing
  
handler :: TChan Message -> AppData -> IO ()
handler chan ad =
  let
    incoming = appSource ad
    --outcoming = appSink ad
  in do 
    print $ show $ appSockAddr ad
    incoming $$ do 
      ln <- await
      case ln >>= parseMsg of 
        Nothing -> return ()
        Just msg -> liftIO $ atomically $ writeTChan chan msg
        
    -- when this conduit is done, connection is closed 

-- sendMessageToPeer :: BCastMessage -> Peer -> IO ()

-- maintains the pool of nodes
-- nodePoolSupervisor :: [Peer] -> IO ()
-- nodePoolSupervisor = do 
--   when (count Peer < 4) 
--      broadcast Addresses to *some* nodes
--      
--   check liveliness of peers in the pool
  
   
runServer :: IO ()
runServer = let 
  settings = serverSettings 5959 "*"
  in do
    putStrLn "Staring on part 5959"
    
    msgChan <- atomically $ newTChan
    forkIO $ commanderThread 0 msgChan

    runTCPServer settings (handler msgChan)
