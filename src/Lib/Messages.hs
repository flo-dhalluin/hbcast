{-# LANGUAGE DeriveGeneric #-}

module Lib.Messages ( Peer (..) , BCastMessage(..)) where

import Data.Serialize
import Data.ByteString
import GHC.Generics

data Peer = Peer { peerPort :: Int
                  , peerAddress :: ByteString
                  , peerId :: ByteString } deriving (Show, Generic)

instance Serialize Peer

-- neat , we could just now encode/decode Peers from bytestrings

data BCastMessage = Addr Peer -- oh hai, here is my address
                  | AddrAck Peer -- good, here's mine
                  | Addresses -- dude, can you share some addresses ?
                  | AddressesBook [Peer]
                  | Ping      -- still here ?
                  | Pong      -- yep
                  deriving (Show, Generic)

instance Serialize BCastMessage
