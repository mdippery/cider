module Main where

import Data.Version          (showVersion)
import System.Environment    (getArgs, getProgName)
import System.Exit           (ExitCode(..), exitWith)
import Text.Printf           (printf)

import qualified Paths_pyxis as P

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
    (ip:_) ->
      putStrLn ip
    _ -> do
      help
      die 1
