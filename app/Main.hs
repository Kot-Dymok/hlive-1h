module Main where

import Lib

import Control.Monad
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Debug.Trace

main :: IO ()
main = do
  startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = void $ do
  return window # set title "Game of Live"
  let
    defaultHeight = 550
    defaultWidth  = defaultHeight * 2
    scaleFactor = 10
    defaultLengthX = 5 -- defaultWidth `div` scaleFactor
    defaultLengthY = 5 -- defaultHeight `div` scaleFactor
    defaultTimerStep = ceiling $ 0.1 * 1000 -- in ms
    defaultGrid = replicate defaultLengthX $ replicate defaultLengthY False 
    defaultGame = Game defaultGrid [3] [2, 3]

  canvas       <- UI.canvas
                  # set UI.width  defaultWidth
                  # set UI.height defaultHeight
                  # set style [("border", "solid black 1px"), ("background", "#fff")]
  timer        <- UI.timer # set UI.interval defaultTimerStep
  activate     <- UI.button #+ [string "Run/Pause"]
  nX           <- UI.input  # set value (show defaultLengthX)
  nY           <- UI.input  # set value (show defaultLengthY)
  changeX      <- UI.input  # set value (show 0)
  changeY      <- UI.input  # set value (show 0)
  changeValue  <- UI.input  # set value (show False)
  changeButton <- UI.button #+ [string "Cnange"]
  clearCanvas  <- UI.button #+ [string "Clear the canvas."]
  clearGrid    <- UI.button #+ [string "Clear the grid."]

  getBody window #+
    [ row [
        column [ element clearCanvas
               , element activate
               ],
        column [ row [string "Rows:" # set style [("width","60px")], element nY]
               , row [string "Columns:" # set style [("width","60px")], element nX]
               ],
        column [ row [string "X" # set style [("width","60px")], element changeX]
               , row [string "Y" # set style [("width","60px")], element changeY]
               ],
        column [ row [string "Value" # set style [("width","60px")], element changeValue]
               , element changeButton
               ]
        ],
        element canvas
    ]
  UI.start timer

  nXValue <- stepper "" $ UI.valueChange nX
  nYValue <- stepper "" $ UI.valueChange nY
  changeXValue <- stepper "" $ UI.valueChange changeX
  changeYValue <- stepper "" $ UI.valueChange changeY
  changeVValue <- stepper "" $ UI.valueChange changeValue
  -- mousePosition <- stepper (0, 0) $ UI.mousemove canvas

  isActive <- accumB False (not <$ UI.click activate)
  let
    xyvValues = liftA3 (\x y v -> (x, y, v)) changeXValue changeYValue changeVValue
    nXYValues = liftA2 (\x y -> (x, y)) nXValue nYValue
    changeGrid = (\(x, y, v) g -> setValue g (read x) (read y) (read v)) <$> xyvValues <@ (UI.click changeButton)
    clearBehaviour :: UI.Event (Game -> Game)
    clearBehaviour = (\(nx, ny) g -> g {Lib.grid = replicate (read nx) $ replicate (read ny) False}) <$> nXYValues <@ (UI.click clearGrid)
    getBin :: (Int, Int) -> (Int, Int) -> Game -> (Int, Int)
    getBin (mx, my) (w, h) (Game g _ _) = (x, y)
      where
        w' = length g
        h' = length $ head g
        x = floor $ fromIntegral w / fromIntegral mx * fromIntegral w'
        y = floor $ fromIntegral h / fromIntegral my * fromIntegral h'

    mouseChange :: UI.Event (Game -> Game)
    mouseChange = (\xy g -> let (x', y') = getBin xy (defaultWidth, defaultHeight) g in traceShow (x', y', g) $ revertValue g x' y') <$> (UI.mousedown clearGrid)
    doAutoStep = whenE isActive $ step . traceShowId <$ (UI.tick  timer)

    unionWith' :: [UI.Event a] -> UI.Event a
    unionWith' [] = undefined
    unionWith' (e1:es) | length es == 1 = UI.unionWith const e1 $ head es
                       | otherwise      = UI.unionWith const e1 $ unionWith' es
    summaryChanges = unionWith' [clearBehaviour, mouseChange, doAutoStep, changeGrid]
  gameChangeEvent <- accumE defaultGame summaryChanges
  
  on UI.click clearCanvas $ const $
    canvas # UI.clearCanvas

  onEvent gameChangeEvent (\g -> showGameGrid canvas defaultWidth defaultHeight g)

showGameGrid :: UI.Element -> Int -> Int -> Game -> UI ()
showGameGrid canvas w h (Game g _ _) = forM_ fieldAsList (writeRect canvas) 
  where
    w' = length g
    h' = length $ head g
    ceilW = fromIntegral w / fromIntegral w'
    ceilH = fromIntegral h / fromIntegral h'
    writeRect canvas (x, y, c) = do
      let colorName = if c == True then "black" else "white" 
      canvas # set' UI.fillStyle (UI.htmlColor colorName)
      canvas # UI.fillRect (fromIntegral x * ceilW, fromIntegral y * ceilH) ceilW ceilH
    fieldAsList = [(x, y, g !! x !! y) | x <- [0..w'-1], y <- [0..h'-1]]
