{-# LANGUAGE OverloadedStrings #-}

module Lib.Server (runServer) where

import Data.Conduit
import Data.Conduit.Network
import Data.Conduit.Cereal
import qualified Data.UUID as UUID
import qualified Data.Map as Map
import Data.Serialize (get, put)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Extra (whenJust)
import Control.Monad (forever, liftM)

import Control.Concurrent (forkIO, threadDelay)

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM (atomically)

import qualified Data.ByteString.Char8 as BC
import Data.Char (toUpper)
import Data.Maybe
import Data.UUID.V1 (nextUUID)
import Lib.Messages

data RemotePeer = RemotePeer
  { remotePeerChan :: TChan BCastMessage -- send messages to that Peer
  , remotePeerInfo :: Peer }             -- coordinates of that Peer

data NodeState = NodeState
  { nodeStatePeer :: Peer
  , nodeStatePool :: TVar (Map.Map BC.ByteString RemotePeer)
  }

-- outgoing part
chanForwader st chan = do
  yield $ AddrAck $ nodeStatePeer st
  forever $ liftIO (atomically $ readTChan chan) >>= yield

-- incoming part : too much liftIO in there
handleNode st peerInfo =  awaitForever $ \msg ->
  let
    rpeer = liftM (Map.lookup $ peerId peerInfo) $ readTVarIO $ nodeStatePool st
  in
    case msg of
      Addresses -> do
        p <- liftIO rpeer
        whenJust p $ \peer -> do
          book <- liftIO $ readTVarIO $ nodeStatePool st
          liftIO $ atomically $ writeTChan (remotePeerChan peer) $ AddressesBook (map remotePeerInfo $ Map.elems book)

      Ping -> do
        p <- liftIO rpeer
        whenJust p $ \peer -> do
          liftIO $ print $ "Ping from " ++ (show $ remotePeerInfo peer)
          liftIO $ atomically $ writeTChan (remotePeerChan peer) Pong

      otherwise -> return ()



handler :: NodeState -> AppData -> IO ()
handler st ad =
  let
    incoming = (appSource ad) =$= (conduitGet2 get)
    outcoming = (conduitPut put) =$= appSink ad
  in do
    print $ show $ appSockAddr ad
    (incoming_, msg) <- incoming $$+ await
    case msg of
      Just (Addr p) ->  do
            chan <- newTChanIO
            atomically $ modifyTVar (nodeStatePool st) $ Map.insert (peerId p) (RemotePeer chan p)
            forkIO $ chanForwader st chan $$ outcoming
            incoming_ $$++ handleNode st p
            closeResumableSource incoming_

bootstrap :: NodeState -> Peer -> IO ()
bootstrap nodeState peer = let
    settings = clientSettings (peerPort peer) (peerAddress peer)
  in do
    putStrLn $ "seeding frmo " ++ (show peer)
    runTCPClient settings $ \ ad -> let
        outcoming = (conduitPut put) =$= appSink ad
        incoming = (appSource ad) =$= (conduitGet2 get)
      in do
        putStrLn "cnctd"
        yield (Addr $ nodeStatePeer nodeState) $$ outcoming
        (incoming_, repl) <- incoming $$+ await
        case repl of
          Just (AddrAck peerinfo) -> do
            chan <- newTChanIO
            atomically $ modifyTVar (nodeStatePool nodeState) $ Map.insert (peerId peerinfo) (RemotePeer chan peerinfo)
            forkIO $ chanForwader nodeState chan $$ (conduitPut put) =$= appSink ad
            incoming_ $$+- handleNode nodeState peerinfo


pingLoop :: NodeState -> IO ()
pingLoop st = do
  threadDelay 1000000
  pool <- readTVarIO $ nodeStatePool st
  mapM_ (\p -> atomically $ writeTChan (remotePeerChan p) Ping) pool
  pingLoop st

makeInitState :: String -> Int -> IO NodeState
makeInitState localAddress port = do
  nodePool <- newTVarIO Map.empty
  mid_ <- liftM (UUID.toASCIIBytes . fromJust) nextUUID
  return $ NodeState (Peer port (BC.pack localAddress) mid_) nodePool

runServer :: String -> Int -> Maybe Peer -> IO ()
runServer listenAddress port mseed = let
    settings = serverSettings port "*" -- (show listenAddress)
  in do
    putStrLn $ "Staring on port :" ++ (show port) ++ " @ " ++ listenAddress
    initNodeState <- makeInitState listenAddress port
    bthread <- whenJust mseed $ \p -> ((forkIO $ bootstrap initNodeState p) >> return ())
    forkIO $ pingLoop initNodeState
    runTCPServer settings (handler initNodeState)
