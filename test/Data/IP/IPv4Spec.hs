module Data.IP.IPv4Spec where

import Prelude hiding (length)

import Control.Exception (evaluate)
import Test.Hspec
import Data.IP.IPv4

spec :: Spec
spec = do
  describe "IPAddress" $ do
    describe "==" $ do
      it "returns true if two IP addresses are equal" $ do
        (read "192.168.0.4" :: IPAddress) == (read "192.168.0.4" :: IPAddress) `shouldBe` True

      it "returns false if two IP addresses are not equal" $ do
        (read "192.168.0.4" :: IPAddress) == (read "192.168.0.5" :: IPAddress) `shouldBe` False

    describe "<=" $ do
      it "compares two IP addresses" $ do
        (read "192.168.0.4" :: IPAddress) <= (read "192.168.0.4" :: IPAddress) `shouldBe` True
        (read "192.168.0.4" :: IPAddress) <= (read "192.168.0.5" :: IPAddress) `shouldBe` True
        (read "192.168.1.4" :: IPAddress) <= (read "192.168.0.5" :: IPAddress) `shouldBe` False

    describe "read" $ do
      describe "valid IP addresses" $ do
        it "parses a valid IP address" $ do
          let ip = read "68.42.3.254" :: IPAddress
          show ip `shouldBe` "68.42.3.254"

      describe "invalid IP addresses" $ do
        it "fails to parse an empty string" $ do
          evaluate (read "" :: IPAddress) `shouldThrow` anyException

        it "fails to parse a string that is not an IP address" $ do
          evaluate (read "foo" :: IPAddress) `shouldThrow` anyException

        it "fails to parse an out-of-range IP address" $ do
          evaluate (read "0.0.0.256" :: IPAddress) `shouldThrow` anyException

    describe "show" $ do
      it "returns a string representation of an IP address" $ do
        let ip = read "192.168.0.2" :: IPAddress
        show ip `shouldBe` "192.168.0.2"

    describe "bounded" $ do
      it "returns the minimum IP address" $ do
        (minBound :: IPAddress) `shouldBe` read "0.0.0.0"

      it "returns the maximum IP address" $ do
        (maxBound :: IPAddress) `shouldBe` read "255.255.255.255"

    describe "enum" $ do
      it "should return the next IP address" $ do
        succ (read "192.168.0.4" :: IPAddress) `shouldBe` (read "192.168.0.5")
        succ (read "192.168.0.255" :: IPAddress) `shouldBe` (read "192.168.1.0")

      it "should wrap around if the last IP address has been reached" $ do
        succ (read "255.255.255.255" :: IPAddress) `shouldBe` (read "0.0.0.0")

      it "should return the previous IP address" $ do
        pred (read "192.168.0.4" :: IPAddress) `shouldBe` (read "192.168.0.3")
        pred (read "192.168.0.0" :: IPAddress) `shouldBe` (read "192.167.255.255")

      it "should wrap around if the first IP address has been reached" $ do
        pred (read "0.0.0.0" :: IPAddress) `shouldBe` (read "255.255.255.255")

  describe "Network" $ do
    describe "read" $ do
      it "parses a valid IP address range" $ do
        size (read "192.168.0.0/24" :: Network) `shouldBe` 256

      it "fails to parse an empty string" $ do
        evaluate (read "" :: Network) `shouldThrow` anyException

      it "fails to parse a string that is not a CIDR block" $ do
        evaluate (read "foo" :: Network) `shouldThrow` anyException

      it "fails to parse a string without a block" $ do
        evaluate (read "192.168.0.1/" :: Network) `shouldThrow` anyException

      it "fails to parse a string with an invalid block" $ do
        evaluate (read "192.168.0.0/foo" :: Network) `shouldThrow` anyException

      it "fails to parse a string with an invalid IP address" $ do
        evaluate (read "192.168.0.256/24" :: Network) `shouldThrow` anyException

      it "fails to parse a string with an out-of-range block" $ do
        evaluate (read "192.168.0.0/-1" :: Network) `shouldThrow` anyException
        evaluate (read "192.168.0.0/33" :: Network) `shouldThrow` anyException

    describe "show" $ do
      it "returns a string representation of an IP address range" $ do
        let r = read "192.168.0.0/30" :: Network
        show r `shouldBe` "192.168.0.0/30"

    describe "size" $ do
      it "returns the number of IP addresses in the range" $ do
        let r27 = read "192.168.0.0/27" :: Network
            r32 = read "192.168.0.0/32" :: Network
        size r27 `shouldBe` 2 ^ (32 - 27)
        size r32 `shouldBe` 2 ^ (32 - 32)

  describe "networkPrefix" $ do
    it "returns the base network address in a range" $ do
      let range1    = read "202.54.1.2/27" :: Network
          range2    = read "192.168.0.0/29" :: Network
          range3    = read "192.168.1.0/24" :: Network
          expected1 = read "202.54.1.0" :: IPAddress
          expected2 = read "192.168.0.0" :: IPAddress
          expected3 = read "192.168.1.0" :: IPAddress
      networkPrefix range1 `shouldBe` expected1
      networkPrefix range2 `shouldBe` expected2
      networkPrefix range3 `shouldBe` expected3

  describe "subnetMask" $ do
    it "returns the network mask in a range" $ do
      let range1    = read "202.54.1.2/27" :: Network
          range2    = read "192.168.0.0/29" :: Network
          range3    = read "192.168.1.0/24" :: Network
          range4    = read "172.16.0.0/12" :: Network
          expected1 = "255.255.255.224"
          expected2 = "255.255.255.248"
          expected3 = "255.255.255.0"
          expected4 = "255.240.0.0"
      (show . subnetMask) range1 `shouldBe` expected1
      (show . subnetMask) range2 `shouldBe` expected2
      (show . subnetMask) range3 `shouldBe` expected3
      (show . subnetMask) range4 `shouldBe` expected4

  describe "wildcardMask" $ do
    it "returns the wildcard mask for an range" $ do
      let range1    = read "202.54.1.2/27" :: Network
          range2    = read "192.168.0.0/29" :: Network
          range3    = read "192.168.1.0/24" :: Network
          range4    = read "172.16.0.0/12" :: Network
          expected1 = "0.0.0.31"
          expected2 = "0.0.0.7"
          expected3 = "0.0.0.255"
          expected4 = "0.15.255.255"
      (show . wildcardMask) range1 `shouldBe` expected1
      (show . wildcardMask) range2 `shouldBe` expected2
      (show . wildcardMask) range3 `shouldBe` expected3
      (show . wildcardMask) range4 `shouldBe` expected4

  describe "broadcastAddress" $ do
    it "returns the broadcast address in a range" $ do
      let range1    = read "172.16.0.0/12" :: Network
          range2    = read "192.168.1.0/24" :: Network
          range3    = read "202.54.1.2/27" :: Network
          expected1 = "172.31.255.255"
          expected2 = "192.168.1.255"
          expected3 = "202.54.1.31"
      (show . broadcastAddress) range1 `shouldBe` expected1
      (show . broadcastAddress) range2 `shouldBe` expected2
      (show . broadcastAddress) range3 `shouldBe` expected3

  describe "contains" $ do
    it "returns true if an IP address is contained within a range" $ do
      let x = read "192.168.0.0/24" :: Network
          ip = read "192.168.0.255" :: IPAddress
      x `contains` ip `shouldBe` True

    it "returns false if an IP address is not contained within a range" $ do
      let x = read "192.168.0.0/24" :: Network
          ip = read "192.168.1.0" :: IPAddress
      x `contains` ip `shouldBe` False
