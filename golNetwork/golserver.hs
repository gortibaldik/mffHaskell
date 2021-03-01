import Control.Concurrent
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan
import qualified Control.Exception as E
import Control.Monad
import Data.List
import qualified Data.Vector as V
import Network.Socket
import System.IO
import Text.Read

boardSize = (50, 50) :: (Int, Int)

boardDataSize = x * y
  where
    (x, y) = boardSize

type Board = V.Vector Bool

newBoard = V.replicate boardDataSize False

boardIdx ix iy = ix + x * iy
  where
    (x, _) = boardSize

boardXY idx = (idx `mod` x, idx `div` x)
  where
    (x, _) = boardSize

inBoard ix iy = ix >= 0 && ix < x && iy >= 0 && iy < y
  where
    (x, y) = boardSize

fieldChar False = '.'
fieldChar True = 'x'

boardFlip ix iy b =
  if inBoard ix iy
    then b V.// [(idx, not (b V.! idx))]
    else b
  where
    idx = boardIdx ix iy

boardPrint b =
  putStrLn "---" >>
  forM_
    [0 .. y - 1]
    (\i -> putStrLn $ map fieldChar $ V.toList $ V.slice (x * i) x b)
  where
    (x, y) = boardSize

boardStep b = V.generate boardDataSize (uncurry cell . uncurry val . boardXY)
  where
    val ix iy =
      ( b V.! boardIdx ix iy
      , genericLength
          [ ()
          | x <- [ix - 1 .. ix + 1]
          , y <- [iy - 1 .. iy + 1]
          , inBoard x y
          , b V.! boardIdx x y
          ])
    cell True val = val == 3 || val == 4
    cell False val = val == 3

data InMsg
  = DoPoll
  | DoStep
  | DoFlip Int
           Int
  | DoTerminate

data OutMsg =
  SetBoard (V.Vector Bool)

data ServerCom = ServerCom
  { inChan :: Chan InMsg
  , outChan :: TChan OutMsg
  }

newServerCom = ServerCom <$> newChan <*> newBroadcastTChanIO

workerThread :: ServerCom -> Board -> IO ()
workerThread com board = do
  let continue b = workerThread com b
      broadcast b = atomically $ writeTChan (outChan com) $ SetBoard b
      cb b = boardPrint b >> broadcast b >> continue b
  msg <- readChan (inChan com)
  case msg of
    DoPoll -> cb board
    DoStep -> cb $ boardStep board
    DoFlip x y -> cb $ boardFlip x y board
    DoTerminate -> pure ()

main =
  withSocketsDo $ do
    com <- newServerCom
    worker <- forkIO $ workerThread com newBoard
    E.bracket open close $ mainLoop com
    writeChan (inChan com) DoTerminate
  where
    open = do
      sock <- socket AF_INET Stream 0
      setSocketOption sock ReuseAddr 1
      bind sock $ SockAddrInet 10042 0
      listen sock 10
      return sock

mainLoop com sock =
  forever $ do
    (c, _) <- accept sock
    forkIO $ E.bracket (setupConn c) hClose $ runConn com

setupConn c = do
  h <- socketToHandle c ReadWriteMode
  hSetBuffering h NoBuffering
  return h

runConn com h = do
  myChan <- atomically $ dupTChan (outChan com)
  writeChan (inChan com) DoPoll
  let recvLoop = loop
        where
          loop = do
            cmd <- words . filter (>= ' ') <$> hGetLine h --filter out \r sent by telnet (this was corrected from earlier 'init' that just discarded the last character)
            case cmd of
              ["poll"] -> do
                writeChan (inChan com) DoPoll
                loop
              "flip":[p1, p2] -> do
                let input = DoFlip <$> readMaybe p1 <*> readMaybe p2
                maybe
                  (hPutStrLn h "wrong coords")
                  (writeChan $ inChan com)
                  input
                loop
              ["step"] -> do
                writeChan (inChan com) DoStep
                loop
              ["quit"] -> pure ()
              [] -> loop
              _ -> do
                hPutStrLn h ("error")
                loop
  let sendLoop =
        forever $ do
          SetBoard v <- atomically $ readTChan myChan
          hPutStrLn h $ "board " ++ map fieldChar (V.toList v)
  sender <- forkIO $ sendLoop
  recvLoop
  killThread sender
