module Main where

import qualified Graphics.WorldTurtle as WT
import Control.Monad
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class (lift)

-- Define our vector and operations. These could be removed using a 
-- proper vector lib.

data V3 = V3 { x:: Float, y:: Float, z :: Float}

zero :: V3 
zero = V3 0 0 0

add :: V3 -> V3 -> V3
add a b = V3 (x a + x b) (y a + y b) (z a + z b)

scale :: Float -> V3 -> V3
scale s a = V3 (x a * s) (y a * s) (z a * s)

dot :: V3 -> V3 -> Float
dot a b = (x a * x b) + (y a * y b) + (z a * z b)

neg :: V3 -> V3
neg a = V3 (- x a) (- y a) (- z a)

rotate :: V3 -> V3 -> Float -> V3 
rotate v p angle = 
  let x' = scale (cos angle) v
      y' = scale (sin angle) p
   in add x' y'

rotateDeg :: V3 -> V3 -> Float -> V3 
rotateDeg v p angle = rotate v p (pi * angle / 180)

project' :: V3 -- ^ origin 
         -> Float -- ^ distance L
         -> V3 -- eye x
         -> V3 -- eye y
         -> V3 -- eye z
         -> V3 -- ^ Vector `v` to transform via projection. 
         -> WT.Point -- ^ Resulting 2D point.
project' e l ex ey ez v =
  let r   = add v (neg e)
      ex' = dot ex r 
      ey' = dot ey r
      l'  = l / dot ez r
   in (l' * ex', l' * ey')

project :: V3 -> WT.Point
project = project' (V3 5 5 (-5)) 100 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)

-- Define our 3D Turtle System

data TurtleState = TS { p :: V3, h :: V3, l :: V3, u :: V3 } 

type TurtleCommand3D a = StateT TurtleState WT.TurtleCommand a

runTurtle3D :: TurtleCommand3D () -> IO ()
runTurtle3D command = 
  let emptyState = TS zero (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)
      command' = do -- Jump to initial starting position
        lift $ WT.setPenDown False
        p' <- project . p <$> get  
        lift $ WT.goto p'
        lift $ WT.setPenDown True
        command -- Run passed in command
   in WT.runTurtle $ evalStateT command' emptyState

drawTo :: WT.Point -> WT.TurtleCommand ()
drawTo  (x2, y2) = do 
  (x1, y1) <- WT.position
  let angle    = atan2 (y2 - y1) (x2 - x1) 
  let distance = sqrt $ (x2 - x1)^2 + (y2 - y1)^2
  WT.setHeading (180 * angle / pi)
  WT.forward distance

-- Define our 3D turtle commands

forward :: Float -> TurtleCommand3D ()
forward distance = do
  -- calculate new position
  s <- get
  let h' = scale distance (h s)
  let p' = add (p s) h'
  put s { p = p'}
  -- draw position in 2D
  let newCoord = project p'
  lift $ drawTo newCoord

backward :: Float -> TurtleCommand3D ()
backward distance = forward (- distance)

yaw :: Float -> TurtleCommand3D ()
yaw angle = do
  s <- get
  let h' = rotateDeg (h s) (l s) angle
  let l' = rotateDeg (l s) (neg (h s)) angle
  put s {h = h', l = l'}

pitch :: Float -> TurtleCommand3D ()
pitch angle = do
  s <- get
  let h' = rotateDeg (h s) (u s) angle
  let u' = rotateDeg (u s) (neg (h s)) angle
  put s { h = h', u = u'}

roll :: Float -> TurtleCommand3D ()
roll angle = do
  s <- get
  let l' = rotateDeg (l s) (u s) angle
  let u' = rotateDeg (u s) (neg (l s)) angle
  put s { l = l', u = u' }

-- Draw a 3D cube

main :: IO ()
main = runTurtle3D $ do
  lift $ WT.setVisible True
  pitch 12
  replicateM_ 4 $ do 
    replicateM_ 4 $ do
      forward 10
      yaw 90
    forward 10
    pitch 90