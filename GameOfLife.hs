import Data.String.Utils

data Coord = Coord Int Int

data CellState = Alive | Dead

data Grid = Grid [[CellState]] Int

instance Show Grid where
	show = showGrid

showRow :: [CellState] -> String
showRow = map cellStateToChar
	where
		cellStateToChar Alive = 'X'
		cellStateToChar Dead = '.'

showGrid :: Grid -> String
showGrid (Grid rows _) = join "\n" (map showRow rows)

createGrid :: Int -> Grid
createGrid size = Grid (replicate size $ replicate size Dead) size

makeGrid :: [[CellState]] -> Grid
makeGrid rows
	| areRowsValid = (Grid rows rowCount)
	| otherwise = error "Invalid number of cells"
	where
		rowCount = length rows
		areRowsValid = all (==rowCount) $ map length rows

getCellState :: Grid -> Coord -> CellState
getCellState (Grid rows _) (Coord x y) = rows !! y !! x

getGridSize :: Grid -> Int
getGridSize (Grid _ size) = size

isOnGrid :: Grid -> Coord -> Bool
isOnGrid grid (Coord x y) =
	x >= 0 && x < gridSize && y >= 0 && y < gridSize
	where
		gridSize = getGridSize grid

getNeighborStates :: Grid -> Coord -> [CellState]
getNeighborStates grid (Coord x y) =
	[getCellState grid (Coord nx ny)
		| nx <- [x-1..x+1], ny <- [y-1..y+1],
		nx /= x || ny /= y,
		isOnGrid grid (Coord nx ny)]

getAliveNeighborCount :: Grid -> Coord -> Int
getAliveNeighborCount grid coord = length aliveNeighbors
	where
		aliveNeighbors = filter isCellAlive neighbors
		neighbors = getNeighborStates grid coord

isCellAlive :: CellState -> Bool
isCellAlive Alive = True
isCellAlive Dead = False

nextCellState :: Grid -> Coord -> CellState
nextCellState grid coord
	| aliveNeighbors == 2 = Alive
	| otherwise = Dead
	where aliveNeighbors = getAliveNeighborCount grid coord

nextRowState :: Grid -> Int -> [CellState]
nextRowState grid y = 
	[nextCellState grid (Coord x y)
	| x <- [0..gridSize-1]]
	where
		gridSize = getGridSize grid

nextGridState :: Grid -> Grid
nextGridState grid = (Grid newRows gridSize)
	where
		newRows = [nextRowState grid y | y <- [0..gridSize-1]]
		gridSize = getGridSize grid

run :: Grid -> Int -> IO()
run grid maxIterations = runIteration grid 1
	where
		runIteration grid stepNumber = do
			putStrLn $ (show stepNumber) ++ ">"
			putStrLn $ show grid
			if stepNumber < maxIterations
				then runIteration (nextGridState grid) (stepNumber + 1)
				else return ()

main = do
	let grid = makeGrid [
		[Alive, Alive, Dead],
		[Dead, Dead, Dead],
		[Dead, Dead, Dead]]
	run grid 10