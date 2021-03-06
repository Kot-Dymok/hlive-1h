module Main where

import Lib

import Control.Monad
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

-- import Debug.Trace

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
    defaultTimerStep = ceiling $ 0.01 * 1000 -- in ms
    defaultGrid = replicate defaultLengthX $ replicate defaultLengthY False
    defaultRule = "B3/S23"
    defaultGame = let (b,s) = parseRules defaultRule in Game defaultGrid b s

  canvas       <- UI.canvas
                  # set UI.width  defaultWidth
                  # set UI.height defaultHeight
                  # set style [("border", "solid black 1px"), ("background", "#fff")]
  timer        <- UI.timer # set UI.interval defaultTimerStep
  activate     <- UI.button #+ [string "Run/Pause"]
  nX           <- UI.input  # set value (show defaultLengthX)
  nY           <- UI.input  # set value (show defaultLengthY)
  rule         <- UI.input  # set value defaultRule
  changeX      <- UI.input  # set value (show 0)
  changeY      <- UI.input  # set value (show 0)
  changeValue  <- UI.input  # set value (show False)
  changeButton <- UI.button #+ [string "Cnange"]
  clearCanvas  <- UI.button #+ [string "Clear the canvas."]
  clearGrid    <- UI.button #+ [string "Set rule and clear the grid."]

  getBody window #+
    [ row [
        column [ element clearCanvas, element activate ],
        column [ string "Rows:" , string "Columns:" ],
        column [ element nY, element nX ],
        column [ string "Rule:"],
        column [ element rule, element clearGrid ],
        column [ row [string "X", element changeX]
               , row [string "Y", element changeY]
               ],
        column [ string "Value:"],
        column [ element changeValue, element changeButton ]
        ],
        element canvas
    ]
  UI.start timer

  nXValue <- stepper (show defaultLengthX) $ UI.valueChange nX
  nYValue <- stepper (show defaultLengthY) $ UI.valueChange nY
  ruleValue <- stepper defaultRule $ UI.valueChange rule
  changeXValue <- stepper "0" $ UI.valueChange changeX
  changeYValue <- stepper "0" $ UI.valueChange changeY
  changeVValue <- stepper "False" $ UI.valueChange changeValue
  -- mousePosition <- stepper (0, 0) $ UI.mousemove canvas

  isActive <- accumB False (not <$ UI.click activate)
  let
    xyvValues = liftA3 (\x y v -> (x, y, v)) changeXValue changeYValue changeVValue
    nXYRValues = liftA3 (\x y r -> (x, y, r)) nXValue nYValue ruleValue
    changeGrid = (\(x, y, v) g -> setValue g (read x) (read y) (read v)) <$> xyvValues <@ (UI.click changeButton)
    clearBehaviour :: UI.Event (Game -> Game)
    clearBehaviour = (\(nx, ny, v) g ->
                        let (b,s) = parseRules v
                        in Game (replicate (read nx) $ replicate (read ny) False) b s) <$> nXYRValues <@ (UI.click clearGrid)
    getBin :: (Int, Int) -> (Int, Int) -> Game -> (Int, Int)
    getBin (mx, my) (w, h) (Game g _ _) = (x, y)
      where
        w' = length g
        h' = length $ head g
        x = floor $ fromIntegral mx / fromIntegral w * fromIntegral w'
        y = floor $ fromIntegral my / fromIntegral h * fromIntegral h'

    mouseChange :: UI.Event (Game -> Game)
    mouseChange = (\xy g -> let (x', y') = getBin xy (defaultWidth, defaultHeight) g in revertValue g x' y') <$> (UI.mousedown canvas)
    -- mouseChange = (\xy g ->
    --                  let (x', y') = getBin xy (defaultWidth, defaultHeight) g
    --                  in revertValue g x' y') <$> mousePosition <@ (UI.mousedown canvas)
    doAutoStep = whenE isActive $ step <$ (UI.tick timer)

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
