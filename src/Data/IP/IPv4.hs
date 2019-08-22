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
  Module      : Network.IP.IPv4
  Description : Network-related data types and functions
  License     : LGPL-3
  Maintainer  : michael@monkey-robot.com

  Models networks in Haskell.
-}
module Data.IP.IPv4
  (
    -- * Data types
    IPAddress
  , IPAddressRange
  , NetworkMask

    -- * Operations
  , (.++.)
  , (.:)

    -- * Basic functions
  , addresses
  , length
  , networkAddress
  , subnetMask
  , broadcastAddress

    -- * Searching
  , contains

  , -- temp
  addrAsInt
  ) where

import Prelude hiding (length)
import qualified Prelude as P (length)

import Control.Monad (ap)
import Data.Bits     ((.&.), (.|.), Bits, complement, shift, shiftL, xor)
import Data.Bool     (bool)
import Data.List     (intercalate, nub, sort)
import Data.Word     (Word32)

import Data.List.Split (splitOn)

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

-- | Network mask.
newtype NetworkMask = NetworkMask { maskAsInt :: Word32 }

instance Show NetworkMask where
  show = addressToString . maskAsInt

instance Eq NetworkMask where
  (NetworkMask x) == (NetworkMask y) = x == y

-- | Represents a range of IP addresses.
--
-- Convert a string in CIDR notation to an address using 'read' to create
-- an @IPAddressRange@:
--
-- >>> read "192.168.0.0/24" :: IPAddressRange
newtype IPAddressRange = IPAddressRange
  { -- | List of all IP addresses in the address range.
    addresses :: [IPAddress]
  }

instance Show IPAddressRange where
  show = show . addresses

instance Read IPAddressRange where
  readsPrec _ = maybe [] (\as -> [(IPAddressRange as, "")]) . parseStringToRange

instance Eq IPAddressRange where
  (IPAddressRange xs) == (IPAddressRange ys) = xs == ys

-- | '<>' is a synonym for '.++.'
instance Semigroup IPAddressRange where
  (<>) = (.++.)

-- | 'mempty' returns an empty 'IPAddressRange'
instance Monoid IPAddressRange where
  mempty = IPAddressRange []

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

parseStringToRange :: String -> Maybe [IPAddress]
parseStringToRange s =
  case parseStringToBlock s of
    Nothing -> Nothing
    Just (base, mask)
      | mask > 32 -> Nothing
      | otherwise -> Just $ map IPAddress $ takeWhile f [base ..]
      where
        g = maskAsInt . maskFromBits
        f = (==) (base .&. (g mask)) . (.&.) (g mask)

isOctet :: Word32 -> Bool
isOctet n = n >= 0 && n < 256

maybeOctet :: Word32 -> Maybe Word32
maybeOctet = ap (bool Nothing . Just) isOctet

maskFromBits :: Word32 -> NetworkMask
maskFromBits = NetworkMask . shift 0xffffffff . fromIntegral . (32 -)

-- | Combines two IP addresses into a single range.
(.++.) :: IPAddressRange -> IPAddressRange -> IPAddressRange
lhs .++. rhs = IPAddressRange $ sort $ nub $ addresses lhs ++ addresses rhs

-- | Adds an IP address to an existing range.
(.:) :: IPAddress -> IPAddressRange -> IPAddressRange
addr .: addrs = IPAddressRange $ sort $ nub $ addr : addresses addrs

-- | Calculates the base network address for a range of IP addresses.
networkAddress :: IPAddressRange -> IPAddress
networkAddress xs =
  let first = head $ addresses xs
      mask  = 0xffffff00
   in IPAddress $ (addrAsInt first) .&. mask

-- | Calculates the /subnet mask/ or /network mask/ for a range of
-- IP addresses.
subnetMask :: IPAddressRange -> NetworkMask
subnetMask xs =
  let base  = networkAddress xs
      end   = last $ addresses xs
      d     = (addrAsInt end) - (addrAsInt base)
      n     = round $ logBase 2 (fromIntegral d)
      m     = 0x1 `shiftL` (fromIntegral n)
      bits  = 0xffffffff `xor` (m - 1)
   in NetworkMask bits

-- | Calculates the /broadcast address/ for a range of IP addresses.
broadcastAddress :: IPAddressRange -> IPAddress
broadcastAddress xs =
  let rhs = addrAsInt $ networkAddress xs
      lhs = complement $ maskAsInt $ subnetMask xs
   in IPAddress $ lhs .|. rhs

-- | True if the given address is part of the given address range.
contains :: IPAddressRange -> IPAddress -> Bool
contains = flip elem . addresses

-- | Number of IP addresses contained in the block.
length :: IPAddressRange -> Int
length = P.length . addresses
