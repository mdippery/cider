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
