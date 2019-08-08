{-
Pyxis - A CIDR block calculator
Copyright (C) 2019 Michael Dippery <michael@monkey-robot.com>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}

module Main where

import Data.List             (intercalate)
import Data.Version          (showVersion)
import System.Environment    (getArgs, getProgName)
import System.Exit           (ExitCode(..), exitWith)
import System.IO             (hPutStrLn, stderr)
import Text.Printf           (printf)
import Text.Read             (readMaybe)

import Saturn.Network (IPAddressRange, addresses)

import qualified Paths_pyxis as P

readBlock :: String -> Maybe IPAddressRange
readBlock = readMaybe

help :: IO ()
help = do
  exe <- getProgName
  putStrLn $ printf "Usage: %s <cidr block>" exe

version :: IO ()
version = do
  exe <- getProgName
  let v = showVersion P.version
  putStrLn $ printf "%s v%s" exe v

die :: Int -> IO ()
die 0    = exitWith ExitSuccess
die code = exitWith (ExitFailure code)

main :: IO ()
main = do
  argv <- getArgs
  case argv of
    ("-h":_) ->
      help
    ("--help":_) ->
      help
    ("-V":_) ->
      version
    ("--version":_) ->
      version
    (ip:_) -> do
      case readBlock ip of
        Just r ->
          putStrLn $ intercalate "\n" $ map show $ addresses r
        Nothing -> do
          hPutStrLn stderr $ "Invalid CIDR block: " ++ show ip
          die 2
    _ -> do
      help
      die 1
