
import Data.List.Split
import System.Random
import Control.Monad

type Cell = Bool
data Board = Board [[Cell]] deriving Show

createBoard :: Int -> Int -> Board
createBoard x y = Board (chunksOf x (replicate (x * y) True))

neg :: (Num a, Ord a) => a -> Bool
neg x 
     | x < 0 = True
     | otherwise = False

getDimensions :: Board -> (Int, Int)
getDimensions (Board b)
        | length b == 0 = (0, 0)
        | otherwise = (length b, length (head b))

-- cell at x y
getCellAt :: (Int, Int) -> Board -> Cell
getCellAt (x,y) (Board b)
        | (x >= (length b)) || (neg x) =  False
        | y >= (length (b !! 0)) || (neg y) =  False
        | otherwise     =  (b !! x) !! y

-- get neighbours of cell at x y, cell not included
getNeighbours :: (Int,Int) -> Board -> [Cell]
getNeighbours (x,y) b = map (\x -> getCellAt x b) [(a, b) | a <- [(x-1)..(x+1)], b <- [(y-1)..(y+1)], (x /= a || y /= b)]

-- calculate new state of cell
calculateCell :: (Int,Int) -> Board -> Cell
calculateCell x b = case (countLive x b) of
                        3 -> True
                        2 -> getCellAt x b
                        _ -> False
                where countLive x b = length (filter id (getNeighbours x b))

-- calculate new state of board
step :: Board -> Board
step b = let dim = getDimensions b
         in Board $ chunksOf (snd dim) (map (\x -> calculateCell x b) [(a,c) | a <- [0..((fst dim)-1)], c <- [0..((snd dim)-1)]])

-- list of true
trueList = True : trueList

-- create board with random cells
createRandomBoard :: Int -> IO Board
createRandomBoard x = do
                    g <- newStdGen
                    return (Board (chunksOf x (take (x*x) (randoms g :: [Bool]))))

-- evolve over x steps
evolution :: Int -> Board -> [Board]
evolution 0 b = []
evolution x b = take x (iterate step b)

-- print a cell as a svg rectangle
svgRect :: Int -> Int -> Int -> Int -> Cell -> String
svgRect x y w h cell = "<rect x=\"" ++ show x ++ "\" y=\"" ++ show y ++ "\" width=\"" ++ show w ++ "\" height=\"" ++ show h ++ "\" fill=\"" ++ (fillColor cell) ++ "\" />"
            where fillColor cell
                        | cell = "black"
                        | otherwise = "white"

-- print a board as a svg rectangles
svgCells :: Int -> Board -> String
svgCells x b = unlines (map (\dim -> svgRect ((fst dim) * x) ((snd dim) * x) x x (getCellAt dim b)) [(a,b) | a <- [0..xmax], b <- [0..ymax]])
            where xmax = (fst $ getDimensions b) - 1
                  ymax = (snd $ getDimensions b) - 1
-- print board as svg recangles with begin and end tag
svgBoard :: Int -> Board -> String
svgBoard x b = "<svg width=\"" ++ dimx ++ "\" height=\"" ++ dimy ++ "\" xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\">\n" ++ (svgCells x b) ++ "</svg>"
        where dimx = show $ (fst $ getDimensions b) * x
              dimy = show $ (snd $ getDimensions b) * x

-- print board with links for the previous and next state
htmlSvgBoard :: Int -> Int -> Board -> String
htmlSvgBoard gen x b = "<div><p><a href=\"" ++ show (gen - 1) ++ ".html\">previous</a></p><p><a href=\"" ++ show (gen + 1) ++ ".html\">next</a></p></div>" ++ (svgBoard x b)

-- write a board to a file
writeBoard :: Int -> Int -> FilePath -> Board -> IO ()
writeBoard gen x name b = do
                let res = htmlSvgBoard gen x b
                writeFile name res

-- write a list of boards to a file
writeEvolution :: Int -> Int -> [Board] -> IO ()
writeEvolution gen0 x boards = zipWithM_ step (iterate (+1) gen0) boards
        where step gen board = writeBoard gen x (show gen ++ ".html") board

main = do 
    board <- createRandomBoard 100
    writeEvolution 1 5 (evolution 1000 board)
