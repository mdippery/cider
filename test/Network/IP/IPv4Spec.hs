module Network.IP.IPv4Spec where

import Prelude hiding (length)

import Control.Exception (evaluate)
import Test.Hspec
import Network.IP.IPv4

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

  describe "IPAddressRange" $ do
    describe "read" $ do
      it "parses a valid IP address range" $ do
        length (read "192.168.0.0/24" :: IPAddressRange) `shouldBe` 256

      it "fails to parse an empty string" $ do
        evaluate (read "" :: IPAddressRange) `shouldThrow` anyException

      it "fails to parse a string that is not a CIDR block" $ do
        evaluate (read "foo" :: IPAddressRange) `shouldThrow` anyException

      it "fails to parse a string without a block" $ do
        evaluate (read "192.168.0.1/" :: IPAddressRange) `shouldThrow` anyException

      it "fails to parse a string with an invalid block" $ do
        evaluate (read "192.168.0.0/foo" :: IPAddressRange) `shouldThrow` anyException

      it "fails to parse a string with an invalid IP address" $ do
        evaluate (read "192.168.0.256/24" :: IPAddressRange) `shouldThrow` anyException

      it "fails to parse a string with an out-of-range block" $ do
        evaluate (read "192.168.0.0/-1" :: IPAddressRange) `shouldThrow` anyException
        evaluate (read "192.168.0.0/33" :: IPAddressRange) `shouldThrow` anyException

    describe "show" $ do
      it "returns a string representation of an IP address range" $ do
        let r = read "192.168.0.0/30" :: IPAddressRange
        show r `shouldBe` "[192.168.0.0,192.168.0.1,192.168.0.2,192.168.0.3]"

    describe "length" $ do
      it "returns the number of IP addresses in the range" $ do
        let r27 = read "192.168.0.0/27" :: IPAddressRange
            r32 = read "192.168.0.0/32" :: IPAddressRange
        length r27 `shouldBe` 2 ^ (32 - 27)
        length r32 `shouldBe` 2 ^ (32 - 32)

  describe ".++." $ do
    it "combines two IP address ranges" $ do
      let x = read "192.168.0.0/30" :: IPAddressRange
          y = read "10.10.10.10/30" :: IPAddressRange
          z = x .++. y
      show z `shouldBe` "[10.10.10.10,10.10.10.11,192.168.0.0,192.168.0.1,192.168.0.2,192.168.0.3]"

    it "removes duplicate IP addresses" $ do
      let x = read "192.168.0.0/30" :: IPAddressRange
          y = read "192.168.0.2/30" :: IPAddressRange
          z = x .++. y
      show z `shouldBe` "[192.168.0.0,192.168.0.1,192.168.0.2,192.168.0.3]"

  describe ".:" $ do
    it "adds an IP adress to a range" $ do
      let x = read "192.168.0.0/30" :: IPAddressRange
          y = read "10.10.10.10" :: IPAddress
          z = y .: x
      show z `shouldBe` "[10.10.10.10,192.168.0.0,192.168.0.1,192.168.0.2,192.168.0.3]"

    it "removes duplicate IP addresses" $ do
      let x = read "192.168.0.0/30" :: IPAddressRange
          y = read "192.168.0.2" :: IPAddress
          z = y .: x
      show z `shouldBe` "[192.168.0.0,192.168.0.1,192.168.0.2,192.168.0.3]"

  describe "contains" $ do
    it "returns true if an IP address is contained within a range" $ do
      let x = read "192.168.0.0/24" :: IPAddressRange
          ip = read "192.168.0.255" :: IPAddress
      x `contains` ip `shouldBe` True

    it "returns false if an IP address is not contained within a range" $ do
      let x = read "192.168.0.0/24" :: IPAddressRange
          ip = read "192.168.1.0" :: IPAddress
      x `contains` ip `shouldBe` False
