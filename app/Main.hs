module Main where

import Lib

import Control.Monad
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

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
    defaultLengthX = defaultWidth `div` scaleFactor
    defaultLengthY = defaultHeight `div` scaleFactor
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
  changeButton <- UI.button #+ [string "Run/Pause"]
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
  mousePosition <- stepper (0, 0) $ UI.mousemove canvas

  isActive <- accumB False (not <$ UI.click activate)
  let
    nXYValues = liftA2 (\x y -> (x, y)) nXValue nYValue
    clearBehaviour :: UI.Event (Game -> Game)
    clearBehaviour = (\(nx, ny) g -> g {Lib.grid = replicate (read nx) $ replicate (read ny) False}) <$> nXYValues <@ (UI.click clearGrid)

    unionWith' :: [UI.Event a] -> UI.Event a
    unionWith' [] = undefined
    unionWith' (e1:es) | length es == 1 = UI.unionWith const e1 $ head es
                       | otherwise      = UI.unionWith const e1 $ unionWith' es
    summaryChanges = unionWith' [clearBehaviour]
  worldEvent <- accumE defaultGame summaryChanges
  
  on UI.click clearCanvas $ const $
    canvas # UI.clearCanvas
