{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Main
where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.IO
import Network.Socket
import Control.Concurrent
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan
import Control.Monad.Trans.State
import Control.Monad (when)
import System.Exit
import Options.Applicative
import qualified Data.Vector as V
import Data.Semigroup ((<>))
import Data.List.Split
import qualified Data.Set as S
import Lens.Micro.Platform

type CursorPosition = (Int, Int)
type Board = V.Vector Bool
type UserInput = String

data Cursor = Cursor 
    { _cursorX :: !Int
    , _cursorY :: !Int
    }

makeLenses ''Cursor

data World = World
    { _board :: !Board
    , _cursorPosition :: !Cursor
    , _keysPressed :: S.Set Key
    , _pressedCounter :: Int
    }
    
makeLenses ''World

data AddrPort = AddrPort
    { ipAddr :: String
    ,  port :: String
    }

parseAddrPort :: Parser AddrPort
parseAddrPort = AddrPort <$>
    strOption (
        long "address"
        <> short 'a'
        <> metavar "<IPV4>"
        <> help "IPV4 address of the GOL server")
    <*> strOption (
        long "port"
        <> short 'p'
        <> metavar "<PORT>"
        <> showDefault
        <> value "10042"
        <> help "Port to which to connect")

argsParser = info (parseAddrPort <**> helper)
    ( fullDesc
    <> progDesc "Connect and communicate with GOL server"
    <> header "Client for GOL server")

main :: IO ()
main = do
    addrPort <- execParser argsParser
    sock <- socket AF_INET Stream 0
    addr <- addrAddress . head <$>
            getAddrInfo (Just defaultHints) (Just (ipAddr addrPort)) (Just (port addrPort))
    connect sock addr
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    userInputChan <- newChan
    boardChan <- newTChanIO
    dupUser <- dupChan userInputChan
    dupBoard <- atomically $ dupTChan boardChan

    scanner <- forkIO $ scanner hdl dupBoard
    writer <- forkIO $ writer hdl dupUser


    playWorld boardChan userInputChan hdl scanner
    return () -- actually it'll never get here

data FromServerMsg = Error | WrongCoords | B Board

fieldBool '.' = False
fieldBool 'x' = True

readMsgFromServer :: String -> FromServerMsg
readMsgFromServer "error" = Error
readMsgFromServer "wrong coords" = WrongCoords
readMsgFromServer board = B $ V.fromList $ map fieldBool $ (splitOn " " board) !! 1

scanner :: Handle -> TChan FromServerMsg -> IO ()
scanner hdl boardChan = do
    line <- hGetLine hdl
    case readMsgFromServer line of
        B board -> atomically $ writeTChan boardChan (B board)
        _ -> return ()
    scanner hdl boardChan

writer :: Handle -> Chan UserInput-> IO ()
writer hdl chan = do
    s <- readChan chan
    hPutStrLn hdl s
    case s of
        "quit" -> return ()
        _ -> writer hdl chan

checkPressedKeys :: World -> Chan UserInput -> IO World
checkPressedKeys w userInputChan =
    if S.member (SpecialKey KeySpace) (_keysPressed w) then(
        if view pressedCounter w == 0 then return $! over pressedCounter (+1) w
        else do
            writeChan userInputChan "step"
            return w)
    else return w

updateBoardIO :: TChan FromServerMsg -> Chan UserInput -> Float -> World -> IO World
updateBoardIO boardChan userInputChan _ w = do
    s <- atomically $ tryReadTChan boardChan
    newW <- checkPressedKeys w userInputChan
    case s of
        Nothing -> return newW
        Just (B b) -> return $ set board b newW

inputHandlerIO :: Handle -> ThreadId -> Chan UserInput -> Event -> World -> IO World
inputHandlerIO hdl scanner userInputChan event w = case event of
    (EventKey (SpecialKey KeyUp) Down _ _)      -> return $ handleCursorUp w
    (EventKey (SpecialKey KeyDown) Down _ _)    -> return $ handleCursorDown w
    (EventKey (SpecialKey KeyRight) Down _ _)   -> return $ handleCursorRight w
    (EventKey (SpecialKey KeyLeft) Down _ _)    -> return $ handleCursorLeft w
    (EventKey (SpecialKey KeyEnter) Down _ _)   -> do
                                                    writeChan userInputChan flipMsg
                                                    return w
    (EventKey key@(SpecialKey KeySpace) Down _ _)   -> do
                                                    writeChan userInputChan "step"
                                                    return $! over pressedCounter (const 0) (over keysPressed (S.insert key) w)
    (EventKey key@(SpecialKey KeySpace) Up _ _)     -> return $! over pressedCounter (const 0) (over keysPressed (S.delete key) w)
    (EventKey (SpecialKey KeyEsc) Down _ _)     -> do
                                                    writeChan userInputChan "quit"
                                                    killThread scanner
                                                    threadDelay 1000 -- wait for writer thread to actually finish before closing handle
                                                    hClose hdl
                                                    exitSuccess

    _ -> return w
    where
        (x, y) = ((,) <$> view (cursorPosition . cursorX) <*> view (cursorPosition . cursorY) ) w
        flipMsg = "flip " ++ show x ++ " " ++ show y

handleCursorRight = execState (do
  cursorPosition . cursorX += 1
  cursorPosition . cursorX . filtered (>(fst boardSize - 1)) .= (fst boardSize - 1))

handleCursorLeft = execState $ do
    cursorPosition . cursorX -= 1
    cursorPosition . cursorX . filtered (<0) .= 0

handleCursorDown = execState $ do
  cursorPosition . cursorY += 1
  cursorPosition . cursorY . filtered (>(fst boardSize - 1)) .= (fst boardSize - 1)

handleCursorUp = execState $ do
    cursorPosition . cursorY -= 1
    cursorPosition . cursorY . filtered (<0) .= 0

type Location = (Int, Int)
type Offsets  = (Float, Float)
boardSize :: (Int, Int)
boardSize = (50,50)

boardDataSize :: Int
boardDataSize = x * y
    where (x, y) = boardSize

locToCds :: Offsets -> Float -> Location -> Offsets
locToCds (ox,oy) size (lx, ly) = (oy + fromIntegral ly *size,
                ox - fromIntegral lx*size)

drawWorldIO :: Offsets -> Float -> World -> IO Picture
drawWorldIO offs size world = return $ Pictures [board, pointer]
    where
    board = drawBoard offs size world
    (Cursor px py) = _cursorPosition world
    (lx, ly) = locToCds offs size (py, px)
    pointer = translate (lx + size/25) (ly - size/25) $ circle $ size/2

drawBoard :: Offsets -> Float -> World -> Picture
drawBoard offs size world = Pictures $ map (\(x,y,ct) -> createRectangle (x,y) ct) cells
    where
    (bx, by) = boardSize
    cells = [(i `div` bx, i `mod` by, _board world V.! i) | i <- [0..(bx * by - 1)]]
    createRectangle cLoc cType = translate lx ly $ color (cT cType) $ rectangleSolid size size
        where
        (lx, ly) = locToCds offs size cLoc
        cT False = white
        cT True  = black

window :: Display
window = InWindow "Game of life" (bx * 10, by * 10) (10, 10)
  where (bx, by) = boardSize

playWorld :: TChan FromServerMsg -> Chan UserInput -> Handle -> ThreadId -> IO ()
playWorld boardChan userInputChan hdl scanner = playIO
            window
            orange
            10
            (World (V.replicate boardDataSize False) (Cursor 10 10) S.empty 0)
            (drawWorldIO (500, -500) 20)
            (inputHandlerIO hdl scanner userInputChan)
            (updateBoardIO boardChan userInputChan)
