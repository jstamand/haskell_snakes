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

--Note: blocks live on a grid
blockSize::Int
blockSize = 20

newtype Block = Block (Int,Int) deriving(Show,Eq)
newtype CanvasPosition = CanvasPosition (Double,Double) deriving(Show, Eq)

newtype Color = Color Text deriving(Show)

newtype Border = Border [Block] deriving(Show)

data Direction = LeftDir | RightDir | DownDir | UpDir deriving(Enum,Show, Eq)

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

createSnake:: Block -> Direction -> Color -> Snake
createSnake block dir col = Snake{body=(borderList block dir 0), direction=dir, color=col}

updateSnake :: Snake -> IO (Snake)
updateSnake Snake{ body=(b:bs), direction=dir, color=col} = return $ Snake{ body=(advanceBlock b dir):(followBlock bs b), direction=dir, color=col} 

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
drawSnake Snake{body=blocks, color=Color col} = drawBlocks blocks col

--If two snakes are overlapping, draw the overlapping portion a different color.
drawSnakeOverlap:: Snake -> Snake -> Canvas()
drawSnakeOverlap (Snake {body=blocks1}) (Snake {body=blocks2}) = drawBlocks (intersectBy blockEquality blocks1 blocks2) (pack "purple")
                
drawApple:: Apple -> Canvas()
drawApple Apple{pos=block} =
                drawBlocks [block] (pack "yellow")

drawBorder :: Border -> Canvas()
drawBorder (Border blocks) = drawBlocks blocks "black"

-- draws all of the game elements on the screen
drawScreen :: Apple -> Snake -> Snake -> Border -> Canvas() 
drawScreen apple snake1 snake2 border = do
        drawApple apple
        drawSnake snake1
        drawSnake snake2
        drawSnakeOverlap snake1 snake2
        drawBorder border


{--
Initilial state of all of the game elements
--}
initSnake1::Snake
initSnake1 = Snake [(Block (6,6)), (Block (6,5)), (Block(6,4)), (Block(6,3))] DownDir (Color (pack "red"))

initSnake2::Snake
initSnake2 = Snake [(Block (16,6)), (Block (16,5)), (Block(16,4)), (Block(16,3))] DownDir (Color (pack "blue"))

initApple::Apple
initApple = Apple { pos=Block (4,4) }

initBorder::Border
initBorder = createBorder

-- data objects which define the state of the game
data Apple      = Apple { pos::Block } deriving(Show)
data Snake      = Snake { body::[Block], direction::Direction, color::Color} deriving(Show)
data GameState  = GameState{ snake1::Snake, snake2::Snake, border::Border, apple::Apple } deriving(Show)

{------------------------------------------------------------------------------
Main loop and updating of the game state
------------------------------------------------------------------------------}
main::IO ()
main = blankCanvas 3000 {events=["keypress"]}runGame

runGame :: DeviceContext -> IO ()
runGame context = do    print "starting program"
                        timerBox <- newEmptyMVar
                        forkIO $ waitLoop timerBox
                        inputBox1 <- newEmptyMVar
                        inputBox2 <- newEmptyMVar    
                        forkIO $ inputLoop context inputBox1 inputBox2
                        mainLoop  (GameState{snake1=initSnake1, snake2=initSnake2, apple=initApple, border=initBorder}) context inputBox1 inputBox2 timerBox

{--
Runs on it's own process, after waiting for a specified amount of time, places True in an MVar for the main looping process
--}
waitLoop:: MVar Bool -> IO()
waitLoop mailbox = do
    threadDelay(100 * 1000)
    putMVar mailbox True
    waitLoop mailbox

{--
waits for an event, places one in either snake1 and snake2 MVars
--}
inputLoop:: DeviceContext -> MVar Direction-> MVar Direction -> IO()
inputLoop context mailbox1 mailbox2 = do
	event <- wait context	--wait for an event
	let press_key = case (eType event, eWhich event) of
							("keypress", Just c) -> Just c
							_ -> Nothing
	if (isNothing press_key)
		then do inputLoop context mailbox1 mailbox2
		else do 
            sendSnake1 (fromJust(press_key)) mailbox1
            sendSnake2 (fromJust(press_key)) mailbox2
            inputLoop context mailbox1 mailbox2

sendSnake1:: Int -> MVar Direction -> IO ()
sendSnake1 code mbox = if snake1Key code
                            then do dir <- key2Direction code
                                    putMVar mbox (fromJust dir)
                            else return ()

sendSnake2:: Int -> MVar Direction -> IO ()
sendSnake2 code mbox = if( snake2Key code)
                            then do dir <- key2Direction code
                                    putMVar mbox (fromJust dir)
                            else return ()


key2Direction:: Int -> IO (Maybe Direction)
key2Direction 119 = return $ Just UpDir
key2Direction 115 = return $ Just DownDir
key2Direction 97 = return $ Just LeftDir
key2Direction 100 = return $ Just RightDir
key2Direction 105 = return $ Just UpDir
key2Direction 106 = return $ Just LeftDir
key2Direction 107 = return $ Just DownDir
key2Direction 108 = return $ Just RightDir
key2Direction _ = return Nothing

snake1Key:: Int -> Bool
snake1Key 119 = True
snake1Key 115 = True
snake1Key 97 = True
snake1Key 100 = True
snake1Key _ = False

snake2Key::Int -> Bool
snake2Key 105 = True
snake2Key 106 = True 
snake2Key 107 = True 
snake2Key 108 = True 
snake2Key _ = False



mainLoop :: GameState -> DeviceContext -> (MVar Direction) -> (MVar Direction) -> (MVar Bool) -> IO ()
mainLoop gamestate context keyBox1 keyBox2 timerBox = do
        let theBorder = border gamestate
        let theSnake1 = snake1 gamestate
        let theSnake2 = snake2 gamestate
        let theApple = apple gamestate
        send context $ do
                clearCanvas
                drawScreen theApple theSnake1 theSnake2 theBorder

        -- update the snakes direction
        maybeDir1 <- tryTakeMVar keyBox1
        maybeDir2 <- tryTakeMVar keyBox2
        theSnake1 <- changeDir theSnake1 maybeDir1
        theSnake2 <- changeDir theSnake2 maybeDir2

         -- verify that the snake hasn't crossed the border
        maybeTime <- tryTakeMVar timerBox
        if(isNothing maybeTime)
            then mainLoop (GameState{snake1=theSnake1, snake2=theSnake2, apple=theApple, border=initBorder}) context keyBox1 keyBox2 timerBox
            else advanceGame (GameState{snake1=theSnake1, snake2=theSnake2, apple=theApple, border=initBorder}) context keyBox1 keyBox2 timerBox

advanceGame :: GameState -> DeviceContext -> (MVar Direction) -> (MVar Direction) -> (MVar Bool) -> IO()
advanceGame gamestate context keyBox1 keyBox2 timerBox = do
    let theSnake1 = snake1 gamestate
    let theSnake2 = snake2 gamestate
    let theApple = apple gamestate
    let theBorder = border gamestate
  
    --save the tail end of the snake in case we need it later
    tail1 <- getTail theSnake1
    tail2 <- getTail theSnake2


    --advance the snake
    theSnake1 <- updateSnake theSnake1
    theSnake2 <- updateSnake theSnake2

    growing1 <- appleEaten theSnake1 theApple
    growing2 <- appleEaten theSnake2 theApple    

    theSnake1 <- growSnake theSnake1 tail1 growing1
    theSnake2 <- growSnake theSnake2 tail2 growing2 

    theApple <- updateApple theApple theSnake1 growing1
    theApple <- updateApple theApple theSnake2 growing2


    --verify that the snake hasn't crossed the boundary
    gameOver <- snakeInBoundary theSnake1 theBorder
    if ( gameOver == False)
        then do print "Snake Intersect!"
                exitSuccess
        else return ()
    gameOver <- snakeInBoundary theSnake2 theBorder
    if ( gameOver == False)
        then do print "Snake Intersect!"
                exitSuccess
        else return ()


    --verify that the snake hasn't hit itself
    gameOver <- snakeHit theSnake1
    if( gameOver == False)
        then do print "Snake Hit!"
                exitSuccess
        else return ()
    gameOver <- snakeHit theSnake2
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

    mainLoop (GameState{snake1=theSnake1, snake2=theSnake2, apple=theApple, border=initBorder}) context keyBox1 keyBox2 timerBox

getTail:: Snake -> IO([Block])
getTail (Snake {body=blocks, direction=dir}) = return (Data.List.drop ((Data.List.length blocks)-1) blocks)

snakeHit:: Snake -> IO(Bool)
snakeHit (Snake {body=blocks, direction=dir}) = return $ Data.List.null $ intersectBy blockEquality (Data.List.take 1 blocks) (Data.List.drop 1 blocks)

appleEaten::Snake -> Apple -> IO(Bool)
appleEaten (Snake {body=blocks, direction=dir}) (Apple apple) = return $ not $ Data.List.null $ intersectBy blockEquality (Data.List.take 1 blocks) [apple]


growSnake:: Snake -> [Block] -> Bool -> IO(Snake)
growSnake (Snake {body=blocks, direction=dir, color=col}) tail bool = if(bool == True)
                                                                then return (Snake {body=blocks++tail, direction=dir, color=col})
                                                                else return (Snake {body=blocks, direction=dir, color=col})


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
changeDir (Snake {body=blocks, direction=LeftDir, color=col}) (Just newDir) = if( newDir == RightDir)
                                                                                then return Snake{body=blocks,direction=LeftDir,color=col}
                                                                                else return Snake{body=blocks,direction=newDir,color=col}
changeDir (Snake {body=blocks, direction=RightDir, color=col}) (Just newDir) = if( newDir == LeftDir)
                                                                                then return Snake{body=blocks,direction=RightDir,color=col}
                                                                                else return Snake{body=blocks,direction=newDir,color=col}
changeDir (Snake {body=blocks, direction=UpDir, color=col}) (Just newDir) = if( newDir == DownDir)
                                                                                then return Snake{body=blocks,direction=UpDir,color=col}
                                                                                else return Snake{body=blocks,direction=newDir,color=col}
changeDir (Snake {body=blocks, direction=DownDir, color=col}) (Just newDir) = if( newDir == UpDir)
                                                                                then return Snake{body=blocks,direction=DownDir,color=col}
                                                                                else return Snake{body=blocks,direction=newDir,color=col}

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

