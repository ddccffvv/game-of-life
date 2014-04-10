
import Data.List.Split
import System.Random
import Control.Monad

type Cell = Bool
data Board = Board [[Cell]] deriving Show

createBoard :: Int -> Int -> Board
createBoard x y = Board (chunksOf x (take (x * y) trueList))

neg :: (Num a, Ord a) => a -> Bool
neg x 
     | x < 0 = True
     | otherwise = False

getDimensions :: Board -> (Int, Int)
getDimensions (Board b)
        | length b == 0 = (0, 0)
        | otherwise = (length b, length (b !! 0))

getCellAt :: Int -> Int -> Board -> Cell
getCellAt x y (Board b)
        | (x >= (length b)) || (neg x) =  False
        | y >= (length (b !! 0)) || (neg y) =  False
        | otherwise     =  (b !! x) !! y

getNeighbours :: Int -> Int -> Board -> [Cell]
getNeighbours x y b = map (\x -> getCellAt (fst x) (snd x) b) [(a, b) | a <- [(x-1)..(x+1)], b <- [(y-1)..(y+1)], (x /= a || y /= b)]

calculateCell :: Int -> Int -> Board -> Cell
calculateCell x y b 
        | countLive x y b == 3 = True
        | countLive x y b == 2 && isLive x y b = True
        | otherwise = False
        where countLive x y b = length (filter (\x -> x == True) (getNeighbours x y b))
              isLive x y b = (getCellAt x y b) == True

step :: Board -> Board
step b = let dim = getDimensions b
         in Board $ chunksOf (snd dim) (map (\x -> calculateCell (fst x) (snd x) b) [(a,c) | a <- [0..((fst dim)-1)], c <- [0..((snd dim)-1)]])

svgRect :: Int -> Int -> Int -> Int -> Cell -> String
svgRect x y w h cell = "<rect x=\"" ++ show x ++ "\" y=\"" ++ show y ++ "\" width=\"" ++ show w ++ "\" height=\"" ++ show h ++ "\" fill=\"" ++ (fillColor cell) ++ "\" />"
            where fillColor cell
                        | cell = "black"
                        | otherwise = "white"

svgCells :: Int -> Board -> String
svgCells x b = unlines (map (\dim -> svgRect ((fst dim) * x) ((snd dim) * x) x x (getCellAt (fst dim) (snd dim) b)) [(a,b) | a <- [0..xmax], b <- [0..ymax]])
            where xmax = (fst $ getDimensions b) - 1
                  ymax = (snd $ getDimensions b) - 1

svgBoard :: Int -> Board -> String
svgBoard x b = "<svg width=\"" ++ dimx ++ "\" height=\"" ++ dimy ++ "\" xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\">\n" ++ (svgCells x b) ++ "</svg>"
        where dimx = show $ (fst $ getDimensions b) * x
              dimy = show $ (snd $ getDimensions b) * x

htmlSvgBoard :: Int -> Int -> Board -> String
htmlSvgBoard gen x b = "<div><p><a href=\"" ++ show (gen - 1) ++ ".html\">previous</a></p><p><a href=\"" ++ show (gen + 1) ++ ".html\">next</a></p></div>" ++ (svgBoard x b)

trueList = True : trueList

createRandomBoard :: Int -> IO Board
createRandomBoard x = do
                    g <- getStdGen
                    return (Board (chunksOf x (take (x*x) (randoms g :: [Bool]))))

writeBoard :: Int -> Int -> FilePath -> IO Board -> IO ()
writeBoard gen x name b = do
                res <- liftM (htmlSvgBoard gen x) b
                writeFile name res

evolution x b
        | x == 0 = []
        | otherwise = b : evolution (x-1) (liftM step b)

writeEvolution gen x (z:[]) = do
            writeBoard gen x ((show gen) ++ ".html") z
writeEvolution gen x (z:xs) = do
            writeBoard gen x ((show gen) ++ ".html") z
            writeEvolution (gen+1) x xs

main = do 
    let x = evolution 100 $ createRandomBoard 30
    writeEvolution 1 10 x
