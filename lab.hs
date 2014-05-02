import Data.List.Split
data Point = Point {x :: Double, y :: Double} deriving (Show, Eq)

data Vec = Vec { start :: Point, end :: Point } deriving (Show, Eq)

add :: Point -> Point -> Point
add (Point x1 y1) (Point x2 y2) = Point (x1 + y1) (x2 + y2)

sub :: Point -> Point -> Point
sub (Point x1 y1) (Point x2 y2) = Point (x1 - y1) (x2 - y2)

dist :: Point -> Double
dist (Point x1 y1) = sqrt $ x1 * x1 + y1 * y1

length :: Vec -> Double
length v = let e = end $ toStart v in dist e

toStart :: Vec -> Vec
toStart a = Vec (Point 0.0 0.0) $ sub (end a) (start a)

connected :: Vec -> Vec -> Bool
connected a b = start a == start b || start a == end b || end a == start b || end a == end b

parallel :: Vec -> Vec -> Bool
parallel vec1 vec2 = let v1 = end $ toStart vec1; v2 = end $ toStart vec2 in x v1 * x v2 + y v1 * y v2 == dist v1 * dist v2

parse :: String -> Vec
parse str = let stuff = splitOn " " str in Vec (Point (read (stuff !! 0)) (read (stuff !! 1))) (Point (read (stuff !! 2)) (read (stuff !! 3)))

makeV :: Vec -> Vec -> Int
makeV vec1 vec2 = if (connected vec1 vec2) && (Main.length vec1 == Main.length vec2) && (not $ parallel vec1 vec2) then 1 else 0

countV :: Vec -> [Vec] -> Int
countV h t = foldl (\acc v -> acc + (makeV h v)) 0 t

doSmart :: [Vec] -> Int
doSmart (h:t) = (countV h t) + doSmart t
doSmart _ = 0

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs)   | x `elem` xs   = rmdups xs
                | otherwise     = x : rmdups xs

main = do
	content <- readFile "data.txt"
	let l = lines content
	return $ doSmart $ map parse $ tail l