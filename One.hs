{-# LANGUAGE OverloadedStrings, DataKinds, ScopedTypeVariables, RecursiveDo, TypeOperators, TemplateHaskell#-}

module Main where
import Graphics.Blank
import Data.Text
import Control.Concurrent
import System.Random
import Data.Maybe
import Data.Char
import Data.List
import System.Exit

blockSize::Int
blockSize = 20

--Note: blocks live on a grid
newtype Block = Block (Int,Int) deriving(Show,Eq)
newtype CanvasPosition = CanvasPosition (Double,Double) deriving(Show, Eq)

newtype Border = Border [Block] deriving(Show)
data Color = Text

data Direction = LeftDir | RightDir | DownDir | UpDir deriving(Enum,Show)

block2Canvas:: Block -> CanvasPosition
block2Canvas (Block (x,y)) = CanvasPosition  (fromIntegral(blockSize)*fromIntegral(x),fromIntegral(blockSize)*fromIntegral(y))

-- the corners of the border
topLeftCorner::Block
topLeftCorner = Block (2,2)

topRightCorner::Block
topRightCorner = Block (32,2)

bottomLeftCorner::Block
bottomLeftCorner = Block (2,32)

bottomRightCorner::Block
bottomRightCorner = Block (32,32)

yDistance::Block -> Block -> Int
yDistance (Block (x1,y1)) (Block (x2,y2)) = abs $ y1-y2

xDistance::Block -> Block -> Int
xDistance (Block (x1,y1)) (Block (x2,y2)) = abs $ x1-x2

-- used to help construct the border

-- Used to construct a data structure containing all the border positions
borderList:: Block -> Direction -> Int -> [Block]
borderList _ _ (-1) = []
borderList (Block (x,y)) LeftDir n = (Block (x,y)) : (borderList (Block (x-1,y)) LeftDir (n-1))
borderList (Block (x,y)) RightDir n = (Block (x,y)) : borderList (Block (x+1,y)) RightDir (n-1)
borderList (Block (x,y)) UpDir n = (Block (x,y)) : borderList (Block (x,y-1)) UpDir (n-1)
borderList (Block (x,y)) DownDir n = (Block (x,y)) : borderList (Block (x,y+1)) DownDir (n-1)

createBorder:: Border
createBorder = Border $ (borderList topLeftCorner RightDir (xDistance topLeftCorner topRightCorner) )   --top border
        ++ (borderList topLeftCorner DownDir (yDistance topLeftCorner bottomLeftCorner))                --left border
         ++ (borderList topRightCorner DownDir (yDistance topRightCorner bottomRightCorner))             --right border
         ++ (borderList bottomLeftCorner RightDir (xDistance bottomLeftCorner bottomRightCorner))        --bottom border

createApple:: Block -> Apple
createApple gridPos = Apple{pos=gridPos}

createSnake:: Block -> Direction -> Snake
createSnake block dir= Snake{body=(borderList block dir 0), direction=dir}

updateSnake :: Snake -> IO (Snake)
updateSnake Snake{ body=(b:bs), direction=dir} = return $ Snake{ body=(advanceBlock b dir):(followBlock bs b), direction=dir} 

advanceBlock:: Block -> Direction -> Block
advanceBlock (Block(x,y)) LeftDir = Block(x-1,y)
advanceBlock (Block(x,y)) RightDir = Block(x+1,y)
advanceBlock (Block(x,y)) UpDir = Block(x,y-1)
advanceBlock (Block(x,y)) DownDir = Block(x,y+1)

followBlock:: [Block] -> Block -> [Block]
followBlock [] _ = []
followBlock [b] update = [update]
followBlock (b:bs) update = update:(followBlock bs b)

{------------------------------------------------------------------------------
Methods to draw things on the canvas
------------------------------------------------------------------------------}


--draws a block on the canvas
drawSegment::CanvasPosition -> Text -> Canvas ()
drawSegment (CanvasPosition (x,y)) color = do 
                beginPath()
                rect (x,y,fromIntegral(blockSize), fromIntegral(blockSize))
                fillStyle color
                fill()
                stroke()
                
-- draws a list of blocks on the canvas
drawBlocks:: [Block] -> Text -> Canvas ()
drawBlocks [] _ = stroke()
drawBlocks [b] color = drawSegment (block2Canvas b) color
drawBlocks (b:bs) color = do
        drawBlocks [b] color
        drawBlocks bs color


drawSnake:: Snake -> Canvas()
drawSnake Snake{body=blocks} = drawBlocks blocks "green" 
                
drawApple:: Apple -> Canvas()
drawApple Apple{pos=block} =
                drawBlocks [block] (pack "yellow")

drawBorder :: Border -> Canvas()
drawBorder (Border blocks) = drawBlocks blocks "black"

drawScreen :: Apple -> Snake -> Border -> Canvas() 
drawScreen apple snake border = do
        drawApple apple
        drawSnake snake
        drawBorder border

{------------------------------------------------------------------------------
Main loop and updating of the game state
------------------------------------------------------------------------------}

initSnake::Snake
initSnake = Snake [(Block (6,6)), (Block (6,5)), (Block(6,4)), (Block(6,3))] RightDir

initApple::Apple
initApple = Apple { pos=Block (4,4) }

initBorder::Border
initBorder = createBorder

-- data objects which define the state of the game
data Apple      = Apple { pos::Block } deriving(Show)
data Snake      = Snake { body::[Block], direction::Direction} deriving(Show)
data GameState  = GameState{ snake::Snake, border::Border, apple::Apple } deriving(Show)

main::IO ()
main = blankCanvas 3000 {events=["keypress"]}runGame

runGame :: DeviceContext -> IO ()
runGame context = do  
        --hSetBuffering stdin NoBuffering --remove buffering on stdin, as per: https://www.haskell.org/pipermail/beginners/2010-January/003287.html
	putStrLn "starting program"
	timerBox <- newEmptyMVar
	forkIO $ waitLoop timerBox
	--make empty MVar
	--forkIO the MVar process
	inputBox <- newEmptyMVar
--    inputBox2 <- newEmptyMVar
	forkIO $ inputLoop context inputBox
	mainLoop  (GameState{snake=initSnake, apple=initApple, border=initBorder}) context inputBox timerBox

waitLoop:: MVar Bool -> IO()
waitLoop mailbox = do
--    print "timingLoop"
    threadDelay(100 * 1000)
    putMVar mailbox True
    waitLoop mailbox

inputLoop:: DeviceContext -> MVar Direction-> IO()
inputLoop context mailbox = do
	event <- wait context	--wait for an event
	let press_key = case (eType event, eWhich event) of
							("keypress", Just c) -> Just c
							_ -> Nothing
	if (isNothing press_key)
		then do inputLoop context mailbox
		else do dir <- key2Direction ( fromJust (press_key))
			if ( isNothing dir )
				then do inputLoop context mailbox
				else do putMVar mailbox (fromJust dir)
					inputLoop context mailbox

key2Direction:: Int -> IO (Maybe Direction)
key2Direction 119 = return $ Just UpDir
key2Direction 115 = return $ Just DownDir
key2Direction 97 = return $ Just LeftDir
key2Direction 100 = return $ Just RightDir
key2Direction _ = return Nothing

mainLoop :: GameState -> DeviceContext -> (MVar Direction) -> (MVar Bool) -> IO ()
mainLoop gamestate context keyBox timerBox = do
        let theBorder = border gamestate
        let theSnake = snake gamestate
        let theApple = apple gamestate
        send context $ do
                clearCanvas
                drawScreen theApple theSnake theBorder

        -- update the snakes direction
        maybeDir <- tryTakeMVar keyBox
        theSnake <- changeDir theSnake maybeDir

         -- verify that the snake hasn't crossed the border
        maybeTime <- tryTakeMVar timerBox
        if(isNothing maybeTime)
            then mainLoop (GameState{snake=theSnake, apple=theApple, border=initBorder}) context keyBox timerBox
            else advanceGame (GameState{snake=theSnake, apple=theApple, border=initBorder}) context keyBox timerBox

advanceGame :: GameState -> DeviceContext -> (MVar Direction) -> (MVar Bool) -> IO()
advanceGame gamestate context keyBox timerBox = do
    let theSnake = snake gamestate
    let theApple = apple gamestate
    let theBorder = border gamestate
  
    --save the tail end of the snake in case we need it later
    tail <- getTail theSnake

    --advance the snake
    theSnake <- updateSnake theSnake

    growing <- appleEaten theSnake theApple
    theSnake <- growSnake theSnake tail growing 
    theApple <- updateApple theApple theSnake growing

    --verify that the snake hasn't crossed the boundary
    gameOver <- snakeInBoundary theSnake theBorder
    if ( gameOver == False)
        then do print "Snake Intersect!"
                exitSuccess
        else return ()

    --verify that the snake hasn't hit itself
    gameOver <- snakeHit theSnake
    if( gameOver == False)
        then do print "Snake Hit!"
                exitSuccess
        else return ()


    
--    theApple <- updateApple theApple theSnake

    {--
    if(appleEaten theSnake theApple)
        then do theSnake <- growSnake theSnake tail
                theApple <- updateApple theApple theSnake
                print theApple
                return theApple
        else do return ()
--}

    mainLoop (GameState{snake=theSnake, apple=theApple, border=initBorder}) context keyBox timerBox

getTail:: Snake -> IO([Block])
getTail (Snake {body=blocks, direction=dir}) = return (Data.List.drop ((Data.List.length blocks)-1) blocks)

snakeHit:: Snake -> IO(Bool)
snakeHit (Snake {body=blocks, direction=dir}) = return $ Data.List.null $ intersectBy blockEquality (Data.List.take 1 blocks) (Data.List.drop 1 blocks)

appleEaten::Snake -> Apple -> IO(Bool)
appleEaten (Snake {body=blocks, direction=dir}) (Apple apple) = return $ not $ Data.List.null $ intersectBy blockEquality (Data.List.take 1 blocks) [apple]


growSnake:: Snake -> [Block] -> Bool -> IO(Snake)
growSnake (Snake {body=blocks, direction=dir}) tail bool = if(bool == True)
                                                                then return (Snake {body=blocks++tail, direction=dir})
                                                                else return (Snake {body=blocks, direction=dir})


updateApple:: Apple -> Snake -> Bool -> IO(Apple)
updateApple apple snake bool = if (bool == True)
                                then do x <- randomRIO( 3::Int, 31::Int)
                                        y <- randomRIO( 3::Int, 31::Int)
                                        newApple <- makeApple x y
                                        if (snakeFoundApple snake newApple)
                                            then do updateApple apple snake bool
                                            else return newApple
                                else return apple

changeDir::Snake -> Maybe Direction -> IO(Snake)
changeDir snake Nothing = return snake
changeDir (Snake {body=blocks, direction=dir}) newDir = return Snake{body=blocks,direction=fromJust newDir}

snakeInBoundary:: Snake -> Border -> IO(Bool)
snakeInBoundary (Snake {body=blocks, direction=dir}) (Border boundary) = return $ Data.List.null $ intersectBy blockEquality blocks boundary

snakeFoundApple:: Snake -> Apple -> Bool
snakeFoundApple (Snake {body=blocks, direction=dir}) (Apple block) = not $ Data.List.null $ intersectBy blockEquality blocks [block]

makeApple::Int -> Int -> IO (Apple)
makeApple x y = return Apple{pos=Block(x,y)}

appleEquality:: Apple -> Apple -> Bool
appleEquality (Apple block1) (Apple block2) = blockEquality block1 block2

blockEquality:: Block -> Block -> Bool
blockEquality (Block(x1,y1)) (Block(x2,y2)) = (x1==x2 && y1==y2)

