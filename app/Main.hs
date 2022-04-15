import Graphics.Gloss
import System.Random

import Numeric

type PointF = (Float, Float)

main :: IO ()
main =  animate (InWindow "Zen" (800, 600) (5, 5)) (greyN 0.2) (picture2 [(0, 0)])--frame

-- Produce one frame of the animation.
frame :: Float -> Picture
frame timeS = Pictures	
 	-- the red rectangles
	[ Translate 0 150 	backRec
	, Translate 0 0		backRec
	, Translate 0 (-150)	backRec
    
    , Translate 0 0 demoRec

	-- the tree
 	, Translate 0 (-150) $	treeFrac 3 timeS
	]

walker :: Picture 
walker = Pictures 
	[
		Color green (rectangleSolid 10 10)
		
	]

-- One of the red backing rectangles, with a white outline.
backRec :: Picture
backRec	= Pictures
	[ Color red 	(rectangleSolid 400 100)
 	, Color white	(rectangleWire  400 100) ]

demoRec :: Picture 
demoRec = Pictures 
    [
        Color blue (rectangleSolid 50 50)
    ]


-- The color for the outline of the tree's branches.
treeOutline :: Color
treeOutline	= makeColor 0.3 0.3 1.0 1.0


-- The color for the shading of the tree's branches.
--	The Alpha here is set to 0.5 so the branches are partly transparent.
treeColor :: Color
treeColor	= makeColor 0.0 1.0 0.0 0.5

newRandomPos :: Int -> Int
newRandomPos x = 1 * x

randomCoord :: IO Int
randomCoord = getStdRandom $ randomR (-1, 1)

picture :: [PointF] -> Float -> Picture
picture origin timeS = Pictures [translate x y (circle 10) | (x,y) <- nextRandoms1 (round timeS) origin]



picture2 :: [PointF] -> Float -> Picture
picture2 origin timeS = 
	let 
		h : t = randomPointSets (mkStdGen 4243) origin
		hp : tp = h
		(x, y) = hp
		xString = showGFloat (Just 2) x ""
		yString = showGFloat (Just 2) y ""
	in Pictures 
	[
		Translate (-350) (250) 
		$ Scale 0.2 0.2
		$ Text (xString ++ ", " ++ yString)
		,	Translate x y (circle 5)
	]

nextRandoms1 :: Int -> [PointF] -> [PointF]
nextRandoms1 seed origins =
    let   add2d = (\(x1,y1) (x2,y2) -> (x1+x2, y1+y2)) --FUNCTION
          count = length origins
          range = (-5::Float, 5)
          xs    = take count $ randomRs range (mkStdGen (seed+0))
          ys    = take count $ randomRs range (mkStdGen (seed+1))
    in
          zipWith add2d  origins  (zip xs ys)

randomPointUpdates :: StdGen -> (Float, Float) -> Int -> ([PointF], StdGen)
randomPointUpdates gen0 range count =
    if (count <= 0)
      then  ([], gen0)  -- generator unaltered
      else
        let  (dx, gen1)  = randomR range gen0 
             (dy, gen2)  = randomR range gen1
             point       = (dx, dy)
             (rest, gen) = randomPointUpdates gen2 range (count-1)
        in
             (point : rest, gen)

nextRandoms :: StdGen -> [PointF] -> ([PointF], StdGen)
nextRandoms gen0 origins =
    let   add2d   = (\(x1,y1) (x2,y2) -> (x1+x2, y1+y2))
          count   = length origins
          range   = (-5::Float, 5)

          (changes, gen1) = randomPointUpdates gen0 range count
          points = zipWith add2d  origins  changes 
    in
          (points, gen1)   

randomPointSets :: StdGen -> [PointF] -> [[PointF]]
randomPointSets gen0 origins =
    let  (pts1, gen1) = nextRandoms gen0 origins
    in   pts1 : (randomPointSets gen1 pts1)

	
-- randomWalker :: Int -> Int -> Float -> Picture
-- randomWalker x y timeS = do
-- 	randX <- randomCoord
-- 	randY <- randomCoord
-- 	Pictures 
-- 	[
-- 		walker
-- 		, Translate (fromIntegral x) (fromIntegral y)
-- 			$ randomWalker (x + randX) (y + randY) timeS
-- 	]
	

-- The tree fractal.
--	The position of the branches changes depending on the animation time
--	as well as the iteration number of the fractal.
treeFrac :: Int -> Float -> Picture
treeFrac 0 timeS = Blank
treeFrac n timeS
 = Pictures
	[ Color treeColor 	$ rectangleUpperSolid 20 300
	, Color treeOutline	$ rectangleUpperWire  20 300
 	, Translate 0 30
		$ Rotate  (200 * sin timeS / (fromIntegral n) )
		$ Scale   0.9 0.9 
		$ treeFrac (n-1) timeS

	, Translate 0 70
		$ Rotate  (-200 * sin timeS / (fromIntegral n))
		$ Scale	  0.8 0.8 
		$ treeFrac (n-1) timeS
	]