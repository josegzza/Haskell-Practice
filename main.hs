-- ====================
-- Complete the following functions and submit your file to Canvas.
-- ====================
-- Do not change the names of the functions. 
-- Do not change the number of arguments in the functions.
-- If your file cannot be loaded by the Haskell compiler, your submission may be cancelled. 
-- Then, submit only code that works.
-- ====================
-- Grading instructions:
-- There is a series of test cases for each function. In order to state that your function
-- "works as described", your output must be similar to the expected one in each case.

-- === distance ===
distance :: (Double, Double) -> (Double, Double) -> Double
distance (x1,y1) (x2,y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)

-- === shift ===

shift :: [t] -> Int -> [t]
shift [] _ = []

shift lst x 
  | x < 0 = shift ((tail lst) ++ [head lst]) (x + 1)
  | x > 0 = shift ((last lst) : (init lst)) (x - 1)
  | x == 0 = lst

-- === myFilter ===

-- Please note that this function takes two arguments.
-- The first one is a function of type (t -> Bool)
-- The second one is a list of a generic type, t.
myFilter :: (t -> Bool) -> [t] -> [t]
myFilter _ [] = []
myFilter f (x : xs) = if (f x)
  then x : (myFilter f xs)
  else (myFilter f xs)

-- === crossover ===

crossover :: [Char] -> [Char] -> Int -> ([Char], [Char])
crossover [] [] _ = ([],[])
crossover (x1:xs1) (x2:xs2) z = if z == 1
  then (x1:xs2, x2:xs1)
  else let (a, b) = crossover xs1 xs2 (z - 1) in  
    (x1:a, x2:b)  

-- === xSort ===

xSort :: [(String, Int, Int)] -> [(String, Int, Int)]
xSort [] = []
xSort ((s, p, g):rest) = let
  right = filter (\(team, points, goals) -> if (points == p) then (goals < g) else (points < p)) rest
  left = filter (\(team, points, goals) -> if (points == p) then (goals >= g) else (points > p)) rest
  in
  (xSort left) ++ [(s, p, g)] ++ (xSort right)

-- === treeSum ===

data Tree = Tree Int Tree Tree | E deriving Show

treeSum :: Tree -> Int
treeSum (E) = 0
treeSum (Tree n lBranch rBranch) = (treeSum lBranch) + n + (treeSum rBranch)

-- === Test cases ===

main = do
    print "=== distance ==="
    print $ distance (10, 20) (5, 15) -- 7.0710678118654755
    print "=== shift ==="
    print $ shift [3, 5, 1, 4, 2] 3 -- [1,4,2,3,5]
    print $ shift [3, 5, 1, 4, 2] (-3) -- [4,2,3,5,1]
    print $ shift "helloworld" 75 -- "worldhello"
    print "=== myFilter ==="
    print $ myFilter (> 3) [1, 2, 3, 4, 5, 6, 7] -- [4,5,6,7]
    print $ myFilter (\x -> x * 2 < 5) [1, 2, 3, 4, 5, 6, 7] -- [1,2]
    print "=== crossover ==="
    print $ crossover "aaaaa" "bbbbb" 3 -- ("aaabb","bbbaa")
    print $ crossover "aaaaa" "bbbbb" 1 -- ("abbbb","baaaa")
    print $ crossover "aaaaa" "bbbbb" 4 -- ("aaaab","bbbba")
    print "=== qSort ==="
    print $ xSort [("Pumas", 10, 3), ("America", 10, 5), ("Chivas", 11, 8), ("Cruz Azul", 11, 2), ("Tigres", 9, 4), ("Rayados", 9, 6)] -- [("Chivas",11,8),("Cruz Azul",11,2),("America",10,5),("Pumas",10,3),("Rayados",9,6),("Tigres",9,4)]
    print "=== treeSum ==="
    print $ treeSum (Tree 8 (Tree 5 (Tree 2 E E) (Tree 7 E E)) (Tree 9 E E))