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

module Main where

import Prelude hiding (read)

import Data.Version          (showVersion)
import System.Environment    (getArgs, getProgName)
import System.Exit           (ExitCode(..), exitSuccess, exitWith)
import System.IO             (hPutStrLn, stderr)
import Text.Printf           (printf)
import Text.Read             (readMaybe)

import Data.IP.IPv4 (Network, usableRange)

import qualified Paths_cider as P

read :: String -> Maybe Network
read = readMaybe

help :: IO String
help = printf "Usage: %s <cidr block>" <$> getProgName

banner :: String
banner = unlines
  [
    "         _     __"
  , "   _____(_)___/ /__  _____"
  , " / ___/ / __  / _ \\/ ___/"
  , " / /__/ / /_/ /  __/ /"
  , " \\___/_/\\__,_/\\___/_/  v%s"
  , "   a network calculator"
  , "  ======================"
  ]

version :: IO String
version = do
  let v = showVersion P.version
  return $ printf banner v

die :: Int -> IO ()
die 0    = exitSuccess
die code = exitWith (ExitFailure code)

main :: IO ()
main = do
  argv <- getArgs
  case argv of
    ("-h":_) ->
      help >>= putStrLn
    ("--help":_) ->
      help >>= putStrLn
    ("-V":_) ->
      version >>= putStrLn
    ("--version":_) ->
      version >>= putStrLn
    (ip:_) ->
      case read ip of
        Just n ->
          putStr $ unlines $ map show $ usableRange n
        Nothing -> do
          hPutStrLn stderr $ "Invalid CIDR block: " ++ show ip
          die 2
    _ ->
      help >>= hPutStrLn stderr >> die 1
