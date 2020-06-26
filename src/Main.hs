{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

module Main
  ( main
  )
where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception.Safe
import Control.Monad
import Data.Aeson
import qualified Data.ByteString as BS
import Data.Function
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock
import Game.Minesweeper.BoardRep
import Game.Minesweeper.Parser
import Game.Minesweeper.Pretty
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
          { std_out = CreatePipe
          , cwd = Just pyHome
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
          { std_out = CreatePipe
          , std_err = CreatePipe
          , cwd = Just pyHome
          }
  (_, Just hOut, Just hErr, ph) <- createProcess cp
  raw <- hGetContents hOut
  -- python reports exit code 120 if we don't consume anything fro err stream here,
  _rawErr <- hGetContents hErr
  ec <- length _rawErr `seq` waitForProcess ph
  case ec of
    ExitSuccess -> pure raw
    ExitFailure 20 -> pure raw
    _ -> do
      putStrLn $ "Process failed: " <> show ec
      pure ""

{-
  IO action for continuous screen capture and board recognition.
  Recognized board is either merged into current existing board (if compatible)
  or replaces the existing board (in case of a new board),
  the new board state is then sent to the solver thread over Chan.
 -}
gameBoardCaptureThread :: FilePath -> T.Text -> Chan (UTCTime, BoardRep) -> IO ()
gameBoardCaptureThread pyHome infoRaw chanBd =
  fix
    (\loop mBoardRep -> do
       r <- tryIO (getCurrentGameBoard pyHome infoRaw)
       case r of
         Right raw | Just brNew <- parseBoard raw -> do
           t <- getCurrentTime
           let br' =
                 fromJust
                   ((do
                       brCur <- mBoardRep
                       mergeBoardRep brNew brCur)
                      <|> Just brNew)
           unless (isPartial br') $ do
             writeChan chanBd (t, br')
           delay >> loop (Just br')
         _ ->
           delay >> loop mBoardRep)
    Nothing
  where
    delay = pure ()

{-
  IO action for solving a board, and marking unrevealed yet safe coordinates
  for the clicker thread.
 -}
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

-- TODO: we should need some mechanism for clicker to not occupy mouse all the time.
{-
  IO action for just clicking on the board like an idiot.
 -}
clickerThread :: Info -> MVar (UTCTime, [Coord]) -> IO ()
clickerThread info mMoves = do
  tInit <- getCurrentTime
  {- note that there is a delay between what this program sees and
     what actually appears on screen, this causes clicker to click on
     tiles that this program considered "not yet revealed" but is actually already revealed.
     clickCache is a bit of a hack to solve this problem: we give this thread some "memory"
     so that it would never click on any tile that are previously clicked (with a window length of 50).
     This might cause trouble when moving towards a new puzzle, but this is the easiest and simple
     solution that I have for now and works most of the time.
   -}
  fix
    (\loop tRef moves clickCache -> do
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
           loop tRef' xs (take 50 (x : clickCache)))
    tInit
    []
    []

_getMouseGlobalLocation :: IO (Int, Int)
_getMouseGlobalLocation = do
  raw <- readProcess "xdotool" ["getmouselocation"] ""
  let xRaw : yRaw : _ = words raw
  pure (read $ drop 2 xRaw, read $ drop 2 yRaw)

_moveMouseToGlobalLocation :: (Int, Int) -> IO ()
_moveMouseToGlobalLocation (x, y) =
  callProcess "xdotool" ["mousemove", show x, show y]

executeStep :: Info -> Coord -> IO ()
executeStep
  Info
    { topLeft = (xBase, yBase)
    , tileSize = (tileWidth, tileHeight)
    , windowId
    }
  (tileR, tileC) = do
    -- TODO: somewhere I messed up row and col, therefore this flipped weird order.
    let x = xBase + tileWidth * tileC + quot tileWidth 2
        y = yBase + tileHeight * tileR + quot tileHeight 2
    callProcess "xdotool" ["mousemove", "--window", show windowId, show x, show y]
    callProcess "xdotool" ["click", "--window", show windowId, "1"]
    -- move to somewhere *not* in the board
    -- otherwise we can get some random tooltip popping up blocking the image
    -- we are trying to recognize.
    callProcess "xdotool" ["mousemove", "--window", show windowId, "100", "100"]

main :: IO ()
main = do
  term <- setupTermFromEnv
  prjHome <- getProjectHome
  let pyHome = prjHome </> "py"
  (infoRaw, info) <- getWindowInfo pyHome
  tInit <- getCurrentTime
  chanBd <- newChan
  mMoves <- newMVar (tInit, [])
  mapM
    async
    [ gameBoardCaptureThread pyHome infoRaw chanBd
    , solverThread term chanBd mMoves
    , clickerThread info mMoves
    ]
    >>= mapM_ wait
