{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

module Main
  ( main,
  )
where

import Control.Applicative
import Control.Concurrent
import qualified Data.Set as S
import Control.Concurrent.Async
import Control.Exception.Safe
import Control.Monad
import Data.Aeson
import qualified Data.ByteString as BS
import Data.Function
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock
import Game.Minesweeper.BoardRep
import Game.Minesweeper.Pretty
import Game.Minesweeper.Parser
import Game.Minesweeper.Solver
import Game.Minesweeper.Types
import Info
import System.Console.Terminfo
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
    ExitFailure 20 -> pure raw
    _ -> do
      putStrLn $ "Process failed: " <> show ec
      pure ""

gameBoardCaptureThread :: FilePath -> T.Text -> Chan (UTCTime, BoardRep) -> IO ()
gameBoardCaptureThread pyHome infoRaw chanBd =
  fix
    ( \loop mBoardRep -> do
        r <- tryIO (getCurrentGameBoard pyHome infoRaw)
        case r of
          Right raw ->
            case parseBoard raw of
              Just brNew -> do
                t <- getCurrentTime
                let br' =
                      fromJust
                        ( ( do
                              brCur <- mBoardRep
                              mergeBoardRep brNew brCur
                          )
                            <|> Just brNew
                        )
                print (isPartial br')
                unless (isPartial br') $ do
                  writeChan chanBd (t, br')
                delay >> loop (Just br')
              Nothing ->
                delay >> loop mBoardRep
          _ ->
            delay >> loop mBoardRep
    )
    Nothing
  where
    delay = pure ()

solverThread :: Terminal -> Chan (UTCTime, BoardRep) -> MVar (UTCTime, [Coord]) -> IO ()
solverThread term chanBd mMoves = fix $ \loop -> do
  (t, br) <- readChan chanBd
  case mkBoard br of
    Nothing -> loop
    Just (xs, bd) ->
      case solveBoard bd xs of
        Just bdFin -> do
          pprBoard term False bdFin
          let solvingSteps =
                concatMap (\(k, v) -> [k | not v])
                  . M.toList
                  $ M.difference (bdMines bdFin) (bdMines bd)
          _ <- swapMVar mMoves (t, solvingSteps)
          loop
        Nothing -> loop

clickerThread :: Info -> MVar (UTCTime, [Coord]) -> IO ()
clickerThread info mMoves = do
  tInit <- getCurrentTime
  fix
    ( \loop tRef moves clickCache -> do
        (tNew, movesNew) <- readMVar mMoves
        let (tRef', moves') =
              if tNew > tRef
                then
                  let cNewMoves = filter (`notElem` clickCache) movesNew
                  in (tNew, cNewMoves)
                else (tRef, moves)
        case moves' of
          [] ->
            threadDelay 10_000 >> loop tRef' moves' clickCache
          x : xs -> do
            executeStep info x
            loop tRef' xs (take 50 (x : clickCache))
    )
    tInit
    []
    []

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
    callProcess "xdotool" ["mousemove", "--window", show windowId, show x, show y]
    callProcess "xdotool" ["click", "--window", show windowId, "1"]
    -- move to somewhere not in the board
    -- otherwise the hover style can interfere a bit.
    callProcess "xdotool" ["mousemove", "--window", show windowId, "100", "100"]

solverLoop :: Terminal -> FilePath -> T.Text -> Info -> IO ()
solverLoop
  term
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
              pprBoard term False bdFin
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
  term <- setupTermFromEnv
  prjHome <- getProjectHome
  let pyHome = prjHome </> "py"
  (infoRaw, info) <- getWindowInfo pyHome
  tInit <- getCurrentTime
  chanBd <- newChan
  mMoves <- newMVar (tInit, [])
  hGameBoard <- async (gameBoardCaptureThread pyHome infoRaw chanBd)
  hSolver <- async (solverThread term chanBd mMoves)
  hClicker <- async (clickerThread info mMoves)
  wait hClicker
  wait hGameBoard
  wait hSolver
