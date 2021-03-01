module Main
where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.Maybe
import Data.List
import Data.Tuple.Extra

main :: IO ()
main = play
    window
    white
    10
    (World (7,7) [] cave Waiting)
    (draw (150, -150) 20 )
    (inputHandler (25, 25) )
    (updateFunc (25,25))
    where
    cave = createMap (25,25)
    updateFunc bounds _ w = case mode w of
        Continuous -> updateRobots bounds w
        Waiting    -> w

type Location = (Int, Int)
type Offsets  = (Float, Float)
type CaveMap  = [[CellType]]

data CellType = Free | Wall | WithRobot
    deriving (Show, Eq)

data Orientation = OUp | ODown | ORight | OLeft
    deriving (Show, Eq)

data Mode = Continuous | Waiting | Add | Remove

data RobotType = Slow | Fast

data Robot = Robot
    { location    :: Location
    , orientation :: Orientation
    , robotType   :: RobotType
    }

data World = World
    { playerLocation    :: Location
    , robots            :: [Robot]
    , caveMap           :: CaveMap
    , mode              :: Mode
    }

window :: Display
window = InWindow "Robot dungeon" (200, 200) (10, 10)

cyclicOrientations :: [(Orientation, Float, (Int, Int))]
cyclicOrientations = let x = (OUp, 0, (-1, 0)): (ORight, 90, (0, 1)):
                             (ODown, 180, (1,0)): (OLeft, 270, (0, -1)): x in x

rotateR :: Orientation -> Orientation
rotateR o = fst3 $ (\(_:x2:_)->x2) $ dropWhile (\t -> fst3 t /= o) cyclicOrientations

rotateL :: Orientation -> Orientation
rotateL o = fst3 $ (\(_:_:_:x4:_)->x4) $ dropWhile (\t->fst3 t/=o) cyclicOrientations 

orToFloat :: Orientation -> Float
orToFloat o = snd3 $ head $ dropWhile (\t->fst3 t/=o) cyclicOrientations

orToTuple :: Orientation -> (Int, Int)
orToTuple o = thd3 $ head $ dropWhile(\t->fst3 t/=o) cyclicOrientations

addTuple :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuple (x1,y1) (x2,y2) = (x1+x2, y1+y2)

applyToTuple :: (Int, Int) -> ( Int -> Int -> a ) -> [[a]]
applyToTuple (a,b) f = [[f x y | y <- [1..b]] | x <- [1..a]]

ifNotThen :: (Maybe Location, Orientation)
            -> (Maybe Location, Orientation)
            -> (Maybe Location, Orientation)
ifNotThen (Just l, o)  _ = (Just l, o)
ifNotThen _ x  = x

robotsQuickSort :: [Robot] -> [Robot]
robotsQuickSort = sortOn (\(Robot (x, y) _ _)->(x,y))
--
-- AI
--
updateRobots :: (Int, Int)  -> World -> World
updateRobots bounds w = newWorld { robots = newRobots}
    where
    nextWorld (oldLoc,newLoc) wld = if oldLoc == newLoc then wld
                                else wld { caveMap = changeMap Add
                                         ( changeMap Remove (caveMap wld) oldLoc Free) newLoc WithRobot }
    f r (wld, rs) = (nw, newRobot:rs)
        where
        newRobot = robotAi (caveMap wld) r
        nw = nextWorld (location r, location newRobot) wld

    (newWorld, newRobots) = foldr f (w, []) (robotsQuickSort $ robots w)
    robotAi mp r@(Robot _ _ Fast) = fastRobotAi bounds mp r
    robotAi mp r@(Robot _ _ Slow) = slowRobotAi bounds mp r


fastRobotAi :: (Int, Int) -> CaveMap -> Robot -> Robot
fastRobotAi bounds mp r = case checkFree bounds mp nextLoc of
    Nothing -> r {orientation = rotateL o}
    _       -> r {location = nextLoc}
    where
    o = orientation r
    nextLoc = location r `addTuple` orToTuple o

slowRobotAi :: (Int, Int) -> CaveMap -> Robot -> Robot
slowRobotAi bounds cm robot = robot { location = nextPos, orientation = nextO}
    where
    pos = location robot
    o = orientation robot
    next1 = ( pos `addTuple` orToTuple ( rotateL o), rotateL o)
    next2 = ( pos `addTuple` orToTuple o, o)
    next3 = ( Just pos, rotateR o)
    func (p, oo) = (checkFree bounds cm p, oo)
    (Just nextPos, nextO) = func next1 `ifNotThen` func next2 `ifNotThen` next3

--
-- MAP UTILS
--
createMap :: (Int, Int) -> CaveMap
createMap tpl = applyToTuple tpl (\x y-> fromMaybe Wall $ checkIn tpl (x,y) >> Just Free)

changeMap :: Mode -> CaveMap -> (Int, Int) -> CellType -> CaveMap
changeMap Remove mp (x,y) nt = modifyAt x (modifyAt y (const nt)) mp
changeMap _ mp (x,y) nt = modifyAt x (modifyAt y (\ct-> if ct == Free then nt else ct)) mp

modifyAt :: Int -> (a->a) -> [a] -> [a]
modifyAt index f l = map (\(x, ix) -> if ix == index then f x else x) $ zip l [1..]

checkIn :: (Int, Int) -> Location -> Maybe (Int, Int)
checkIn (sizex, sizey) (ox, oy)
    | ox == 1 || oy == 1 || ox == sizex || oy == sizey  = Nothing
    | otherwise                                         = Just (ox, oy)

checkFree :: (Int, Int) -> CaveMap -> Location -> Maybe Location
checkFree bounds cm (x,y) = checkIn bounds (x,y) >>= chk ( (cm !! (x-1)) !! (y-1) )
    where
    chk Free a = Just a
    chk _ _    = Nothing

insertRobot :: (Int, Int) -> Location -> RobotType -> World -> World
insertRobot bounds loc rt w = case checkFree bounds mp loc of
    Nothing -> w
    _ -> nw
    where
    mp = caveMap w
    robot = Robot loc OUp rt
    nw = w { caveMap = changeMap Add mp loc WithRobot, robots = robot : robots w}

removeRobot :: (Int, Int) -> [Robot] -> [Robot]
removeRobot pos = filter (\(Robot loc _ _) -> loc /= pos)

--
-- INPUT
--
inputHandler :: (Int, Int) -> Event -> World -> World
inputHandler bounds event w = case event of
    (EventKey (SpecialKey KeyUp) Down _ _)      -> updateWorld OUp
    (EventKey (SpecialKey KeyDown) Down _ _)    -> updateWorld ODown
    (EventKey (SpecialKey KeyRight) Down _ _)   -> updateWorld ORight
    (EventKey (SpecialKey KeyLeft) Down _ _)    -> updateWorld OLeft
    (EventKey (Char 'm') Down _ _)              -> w { caveMap = changeMap Add (caveMap w) ploc Wall }
    (EventKey (Char 'n') Down _ _)              -> w { caveMap = changeMap Remove (caveMap w) ploc Free
                                                     , robots  = removeRobot ploc $ robots w }
    (EventKey (Char 'f') Down _ _)              -> insertRobot bounds ploc Fast w
    (EventKey (Char 's') Down _ _)              -> insertRobot bounds ploc Slow w
    (EventKey (Char 't') Down _ _)              -> updateRobots bounds w
    (EventKey (Char 'c') Down _ _)              -> w { mode = opposite $ mode w}
    _ -> w
    where
    ploc = playerLocation w
    next o = fromMaybe ploc $ checkIn bounds $ ploc `addTuple` orToTuple o
    updateWorld o = w { playerLocation = next o }
    opposite Waiting = Continuous
    opposite Continuous = Waiting
--
-- DRAWING FUNCTIONS
--
locToCds :: Offsets -> Float -> Location -> Offsets
locToCds (ox,oy) size (lx, ly) = (oy + fromIntegral ly *size,
                                    ox - fromIntegral lx*size)

draw :: (Float, Float) -> Float -> World -> Picture
draw offs size world = Pictures [cave, rs, pointer]
    where
    cave = drawMap offs size $ caveMap world
    rs = drawRobots offs size $ robots world
    (lx, ly) = locToCds offs size $ playerLocation world
    pointer = translate (lx + size/25) (ly - size/25) $ circle $ size/2

drawMap :: Offsets -> Float -> CaveMap -> Picture
drawMap offs size c = Pictures $ map (\(x,y,ct) -> createRectangle (x,y) ct) $ concat cells
    where
    cells = [[(x,y, v)|(v, y) <- zip row [1..]]|(row, x) <- zip c [1..]]
    createRectangle cLoc cType = translate lx ly $ color (cT cType) $ rectangleSolid size size
        where
        (lx, ly) = locToCds offs size cLoc
        cT Wall = dark orange
        cT Free    = aquamarine
        cT WithRobot = dark aquamarine

drawRobots :: Offsets -> Float -> [Robot] -> Picture
drawRobots offs size rs = Pictures $ map (drawRobot offs size) rs

drawRobot :: Offsets -> Float -> Robot -> Picture
drawRobot offs size robot = robotPointer
    where
    triangle = polygon [ (-size/3, -size/2),  (size/3, -size/2), (0, size/2) ]
    c Fast = red
    c Slow = blue
    (lx, ly) = locToCds offs size $ location robot
    robotPointer = translate lx ly $
                   rotate ( orToFloat ( orientation robot ) ) $
                   color ( c $ robotType robot )  triangle
