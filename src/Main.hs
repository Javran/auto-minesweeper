{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

module Main
  ( main,
  )
where

import Control.Concurrent
import Control.Exception.Safe
import Control.Monad
import Data.Aeson
import qualified Data.ByteString as BS
import Data.Function
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Game.Minesweeper.Main (pprBoard)
import Game.Minesweeper.Parser
import Game.Minesweeper.Solver
import Game.Minesweeper.Types
import Info
import System.Environment
import System.Exit
import System.FilePath.Posix
import System.IO
import System.Process

getProjectHome :: IO FilePath
getProjectHome = do
  let homeKey = "AUTO_MINESWEEPER_HOME"
  r <- lookupEnv homeKey
  case r of
    Nothing -> do
      putStrLn $ homeKey <> " is not set."
      exitFailure
    Just v -> pure v

getWindowInfo :: FilePath -> IO (T.Text, Info)
getWindowInfo pyHome = do
  let cp =
        (proc (pyHome </> "find_window.py") [])
          { std_out = CreatePipe,
            cwd = Just pyHome
          }
  (_, Just hOut, _, ph) <- createProcess cp
  raw <- BS.hGetContents hOut
  ec <- waitForProcess ph
  case eitherDecodeStrict raw of
    Right v -> pure (decodeUtf8 raw, v)
    Left e -> do
      putStrLn $ "Parse error: " <> show e
      putStrLn $ "Exit status: " <> show ec
      exitFailure

getCurrentGameBoard :: FilePath -> T.Text -> IO String
getCurrentGameBoard pyHome infoRaw = do
  let cp =
        (proc (pyHome </> "get_current_game_board.py") [T.unpack infoRaw])
          { std_out = CreatePipe,
            cwd = Just pyHome
          }
  (_, Just hOut, _, ph) <- createProcess cp
  raw <- hGetContents hOut
  ec <- waitForProcess ph
  case ec of
    ExitSuccess -> pure raw
    ExitFailure 20 -> do
      putStrLn "Some tagging required."
      exitFailure
    _ -> do
      putStrLn $ "Process failed: " <> show ec
      exitFailure

getMouseGlobalLocation :: IO (Int, Int)
getMouseGlobalLocation = do
  raw <- readProcess "xdotool" ["getmouselocation"] ""
  let xRaw : yRaw : _ = words raw
  pure (read $ drop 2 xRaw, read $ drop 2 yRaw)

moveMouseToGlobalLocation :: (Int, Int) -> IO ()
moveMouseToGlobalLocation (x, y) =
  callProcess "xdotool" ["mousemove", show x, show y]

executeStep :: Info -> Coord -> IO ()
executeStep
  Info
    { topLeft = (xBase, yBase),
      tileSize = (tileWidth, tileHeight),
      windowId
    }
  (tileR, tileC) = do
    let x = xBase + tileWidth * tileC + quot tileWidth 2
        y = yBase + tileHeight * tileR + quot tileHeight 2
    print (tileR, tileC)
    callProcess "xdotool" ["mousemove", "--window", show windowId, show x, show y]
    callProcess "xdotool" ["click", "--window", show windowId, "1"]
    -- move to somewhere not in the board
    -- otherwise the hover style can interfere a bit.
    callProcess "xdotool" ["mousemove", "--window", show windowId, "100", "100"]

solverLoop :: FilePath -> T.Text -> Info -> IO ()
solverLoop
  pyHome
  infoRaw
  info@Info {tilesShape = (cols, rows)} =
    fix $ \loop -> do
      rKeepGoing <- tryAny $ do
        boardRaw <- getCurrentGameBoard pyHome infoRaw
        Just tmpBd <- pure (parseBoard boardRaw)
        case mkBoard tmpBd of
          Nothing -> do
            putStrLn "Failed to create the board, exiting..."
            pure False
          Just (xs, bd) -> case solveBoard bd xs of
            Just bdFin -> do
              pprBoard bdFin
              let solvingSteps =
                    concatMap
                      ( \(k@(r, c), v) ->
                          [ k
                            | not v,
                              r >= 0,
                              r < rows,
                              c >= 0,
                              c < cols
                          ]
                      )
                      . M.toList
                      $ M.difference (bdMines bdFin) (bdMines bd)
              loc <- getMouseGlobalLocation
              mapM_ (executeStep info) solvingSteps
              moveMouseToGlobalLocation loc
              pure True
            Nothing -> do
              putStrLn "Failed to create the board, exiting..."
              pure False

      case rKeepGoing of
        Left e -> do
          putStrLn $ "Exception: " <> displayException e
          loop
        Right keepGoing ->
          when keepGoing (threadDelay 3_000_000 >> loop)

main :: IO ()
main = do
  prjHome <- getProjectHome
  let pyHome = prjHome </> "py"
  (infoRaw, info) <- getWindowInfo pyHome
  solverLoop pyHome infoRaw info
