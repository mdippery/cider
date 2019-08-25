{-
Cider - A network calculator
Copyright (C) 2019 Michael Dippery <michael@monkey-robot.com>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE TupleSections #-}


{-|
  Module      : Data.IP.IPv4
  Description : IPv4-related data types and functions
  License     : LGPL-3
  Maintainer  : michael@monkey-robot.com

  Types and functions for working with IPv4 addresses as data.
-}
module Data.IP.IPv4
  (
    -- * Data types
    IPAddress
  , NetworkMask
  , Network

    -- * Basic functions
  , broadcastAddress
  , networkPrefix
  , subnetMask
  , wildcardMask
  , size
  , usableSize

    -- * Ranges
  , range
  , usableRange

    -- * Searching
  , contains
  ) where

import Control.Monad (ap, liftM2)
import Data.Bits ((.&.), (.|.), Bits(..), complement, shift, xor)
import Data.Bool (bool)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.Word (Word32)

import Data.List.Split (splitOn)

import Data.Integer (AsInteger(..))

-- | A 32-bit IPv4 network address.
--
-- Create new IP addresses from their string representations using 'read':
--
-- >>> read "192.168.0.1" :: IPAddress
-- 192.168.0.1
newtype IPAddress = IPAddress { addrAsInt :: Word32 }

instance Show IPAddress where
  show = addressToString . addrAsInt

instance Read IPAddress where
  readsPrec _ = maybe [] (\a -> [(IPAddress a, "")]) . parseStringToAddress

instance Eq IPAddress where
  (IPAddress x) == (IPAddress y) = x == y

instance Ord IPAddress where
  (IPAddress x) `compare` (IPAddress y) = x `compare` y

instance Enum IPAddress where
  toEnum = IPAddress . fromIntegral
  fromEnum = fromIntegral . addrAsInt

instance Bounded IPAddress where
  minBound = read "0.0.0.0"
  maxBound = read "255.255.255.255"

instance Bits IPAddress where
  (IPAddress x) .&. (IPAddress y) = IPAddress $ x .&. y
  (IPAddress x) .|. (IPAddress y) = IPAddress $ x .|. y
  (IPAddress x) `xor` (IPAddress y) = IPAddress $ x `xor` y
  complement = IPAddress . complement . asInteger
  shift (IPAddress x) i = IPAddress x `shift` i
  rotate (IPAddress x) i = IPAddress $ rotate x i
  bitSize = fromJust . bitSizeMaybe
  bitSizeMaybe = bitSizeMaybe . asInteger
  isSigned _ = False
  testBit = testBit . asInteger
  bit = IPAddress . shiftL 0xf
  popCount = popCount . asInteger

instance AsInteger IPAddress where
  asInteger = addrAsInt

-- | A 32-bit network mask.
--
-- There is no way to create this data type directly; instead, use
-- 'subnetMask' and 'wildcardMask' with a 'Network' value to determine
-- the various masks for a given network.
newtype NetworkMask = NetworkMask { maskAsInt :: Word32 }

instance Show NetworkMask where
  show = addressToString . maskAsInt

instance Eq NetworkMask where
  (NetworkMask x) == (NetworkMask y) = x == y

instance Bits NetworkMask where
  (NetworkMask x) .&. (NetworkMask y) = NetworkMask $ x .&. y
  (NetworkMask x) .|. (NetworkMask y) = NetworkMask $ x .|. y
  (NetworkMask x) `xor` (NetworkMask y) = NetworkMask $ x `xor` y
  complement = NetworkMask . complement . asInteger
  shift (NetworkMask x) i = NetworkMask $ x `shift` i
  rotate (NetworkMask x) i = NetworkMask $ x `rotate` i
  bitSize = fromJust . bitSizeMaybe
  bitSizeMaybe = bitSizeMaybe . asInteger
  isSigned _ = False
  testBit = testBit . asInteger
  bit = NetworkMask . shiftL 0xf
  popCount = popCount . asInteger

instance AsInteger NetworkMask where
  asInteger = maskAsInt

-- | Represents an IPv4 network or subnet.
--
-- Create these from a string in standard CIDR block notation:
--
-- >>> read "192.168.0.1"/24
-- 192.168.0.1/24
data Network = Network
  {
    -- | The network's routing address or /network prefix/.
    --
    -- This is a 32-bit value that designates the network component of
    -- an IP address. It is used in conjunction with the 'subnetMask' to
    -- separate a network identifier from a host identifier in an IPv4
    -- address.
    --
    -- See <https://en.wikipedia.org/wiki/Subnetwork subnetwork> for more
    -- information.
    networkPrefix :: IPAddress

    -- | The network's subnet mask.
    --
    -- The subnet mask is used to separate the routing address from the host
    -- identifier in an IPv4 address. Given an IP address, the subnet mask
    -- can be bitwise ANDed with the IP address to get the routing address
    -- (also known as the network prefix).
    --
    -- See <https://en.wikipedia.org/wiki/Subnetwork subnetwork> for more
    -- information.
  , subnetMask :: NetworkMask
  }

instance Show Network where
  show net =
    let m   = complement $ maskAsInt $ subnetMask net
        m'  = ((32 -) . round . logBase 2 . fromIntegral) m
        np  = show $ networkPrefix net
     in np ++ "/" ++ show m'

instance Read Network where
  readsPrec _ = maybe [] (\net -> [(net, "")]) . parseStringToNetwork

addressToString :: (Show a, Bits a, Num a) => a -> String
addressToString n = intercalate "." $ map (show . shift') [24, 16, 8, 0]
  where
    shift' x = shift ((0xff `shift` x) .&. n) (-x)

parseStringToAddress :: String -> Maybe Word32
parseStringToAddress = fmap (foldr (.|.) 0 . zipWith (flip shift . (8 *)) [3, 2, 1, 0])
                     . mapM (maybeOctet . read)
                     . splitOn "."

parseStringToBlock :: String -> Maybe (Word32, Word32)
parseStringToBlock s =
  case splitOn "/" s of
    [ip, mask]  -> (, read mask) <$> parseStringToAddress ip
    _           -> Nothing

parseStringToNetwork :: String -> Maybe Network
parseStringToNetwork s =
  case parseStringToBlock s of
    Nothing -> Nothing
    Just (base, mask)
      | mask > 32 -> Nothing
      | otherwise ->
        let
          sm = maskFromBits mask
          np = IPAddress $ base .&. (asInteger sm)
         in Just $ Network np sm

isOctet :: Word32 -> Bool
isOctet = liftM2 (&&) (>= 0) (< 256)

maybeOctet :: Word32 -> Maybe Word32
maybeOctet = ap (bool Nothing . Just) isOctet

maskFromBits :: Word32 -> NetworkMask
maskFromBits = NetworkMask . shift 0xffffffff . fromIntegral . (32 -)

-- | The network's /broadcast address/.
--
-- A UDP datagram sent to a network's <https://en.wikipedia.org/wiki/Broadcast_address broadcast address>
-- will be received by all devices connected to that network.
broadcastAddress :: Network -> IPAddress
broadcastAddress net =
  let lhs = asInteger $ complement $ subnetMask net
      rhs = asInteger $ networkPrefix net
   in IPAddress $ lhs .|. rhs

-- | The network's /wildcard mask/.
--
-- This mask can be used to separate the host identifier from the rest of
-- an IPv4 address. Essentially it is the opposite of the 'wildcardMask',
-- which is used to separate the network prefix from the rest of an
-- IPv4 address.
wildcardMask :: Network -> NetworkMask
wildcardMask = NetworkMask . xor 0xffffffff . asInteger . subnetMask

-- | All of the IP addresses in the network, including its 'networkPrefix'
-- and its 'broadcastAddress'.
range :: Network -> [IPAddress]
range = liftM2 enumFromTo networkPrefix broadcastAddress

-- | All of the /usable/ IP addresses in the network.
--
-- These are the IP addresses that can be /assigned/ to devices on the
-- network. It does not include the 'networkPrefix' and the
-- 'broadcastAddress'.
usableRange :: Network -> [IPAddress]
usableRange = init . tail . range

-- | Total number of addresses in a network.
--
-- This includes the network's base 'networkPrefix' and its 'broadcastAddress',
-- which are not assignable to actual devices on the network. For the number
-- of /usable/ addresses in a network, see 'usableSize'.
size :: Network -> Int
size = length . range

-- | Total number of /usable/ addresses in a network.
--
-- This is the 'size' of the network, minus two unassignable addresses:
-- the network's 'networkPrefix' and 'broadcastAddress'.
usableSize :: Network -> Int
usableSize = length . usableRange

-- | True if the network contains the given IP address.
contains :: Network -> IPAddress -> Bool
contains = flip elem . range
