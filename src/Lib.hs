module Lib
    ( Game (..),
      parseRules,
      step,
      getValue,
      setValue,
      revertValue
    ) where

import Data.List.Split

data Game = Game {
  grid :: [[Bool]],
  born :: [Int],
  survives :: [Int]
  }
  deriving Show


checkBounds :: Int -> Int -> Int
checkBounds x lx | x >= lx = x - lx
                 | x < 0 = lx + x
                 | otherwise = x


getValue :: Game -> Int -> Int -> Bool
getValue (Game g _ _) x y = g !! x' !! y' 
  where
    lx = length g
    ly = length . head $ g
    x' = checkBounds x lx
    y' = checkBounds y ly
                
setValue :: Game -> Int -> Int -> Bool -> Game
setValue g x y v = g {grid = g'}
  where
    lx = length $ grid g
    ly = length . head $ grid g
    x' = checkBounds x lx
    y' = checkBounds y ly
    (beforeX, currX:afterX) = splitAt x' $ grid g
    (beforeY, _:afterY) = splitAt y' currX
    g' = beforeX ++ [beforeY++[v]++afterY] ++ afterX

revertValue :: Game -> Int -> Int -> Game
revertValue g x y = let v' = not $ getValue g x y in setValue g x y v'


nLiveNeighbors :: Game -> Int -> Int -> Int
nLiveNeighbors g x y = length $ filter id neighs
  where
    neighs = [getValue g (x+x') (y+y') | x' <- [-1..1] , y' <- [-1..1], x' /= 0 || y' /= 0]

nextValue :: Game -> Bool -> Int -> Bool
nextValue (Game g b s) v n | v == False && elem n b = True
                           | v == True  && elem n s = True
                           | otherwise              = False

step :: Game -> Game
step g = g {grid = grid'}
  where
    lx = length $ grid g
    ly = length . head $ grid g
    nv :: Int -> Int -> Bool
    nv x' y' = nextValue g v n
      where
        v = getValue g x' y'
        n = nLiveNeighbors g x' y'
    grid' = [[nv x y | y <- [0 .. ly - 1]] | x <- [0 .. lx-1]]

parseRules :: String -> ([Int], [Int])
parseRules string = foldr (\(x,y) (x',y') -> (x++x',y++y')) ([],[]) rules
  where
    substrings = splitOn "/" string
    oneRule :: String -> ([Int], [Int])
    oneRule "" = ([],[])
    oneRule (s:ss) | s == 'B'  = (converted, [])
                   | s == 'S'  = ([], converted)
                   | otherwise = ([],[])
      where
        converted = map read $ chunksOf 1 ss :: [Int]
    rules = map oneRule substrings
