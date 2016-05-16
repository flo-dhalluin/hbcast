{-# LANGUAGE OverloadedStrings #-}

module Main where
import System.Environment
import Control.Monad
import qualified Data.ByteString.Char8 as BC

import Lib.Server
import Lib.Messages
import Network.Socket
import Network.BSD (getHostName)
import Data.IP
import Data.Maybe (fromMaybe)

data ServerOpts = ServerOpts {
    optPort :: Int
  , optSeed :: Maybe Peer
}

parsePeer :: String -> Peer
parsePeer str = let
    (host, port) = break (== ':') str
  in
    Peer (read (tail port)) (BC.pack host) ""

parseArgs :: [String] -> ServerOpts
parseArgs [x] = ServerOpts (read x) Nothing
parseArgs (x:xs) = ServerOpts (read x) (Just $ parsePeer (head xs))
parseArgs _ = ServerOpts 4242 Nothing

getAddressStr :: AddrInfo -> String
getAddressStr addr = case addrAddress addr of
  SockAddrInet _ hostAd -> show $ fromHostAddress hostAd
  SockAddrInet6 _ _ hostAdd6 _ -> show $ fromHostAddress6 hostAdd6
  otherwise -> "*"

getLocalAddress :: IO String
getLocalAddress =
  let
    toString_ = getAddressStr . head
    hints = Just defaultHints {addrFlags = [AI_ADDRCONFIG]}
  in
    do
      lhost <- getHostName
      adinfo <- getAddrInfo hints (Just lhost) Nothing
      return $ toString_ adinfo

main = do
  listenAddress <- getLocalAddress
  opts <- liftM parseArgs getArgs
  runServer listenAddress (optPort opts) (optSeed opts)
