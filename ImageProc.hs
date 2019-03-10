{- 
  File      :  ImageProc.hs 
  Copyright : (c) Hanwen Xu, 03/06/19 
-}
module ImageProc (
    findContours,
    blend,
    autoThresholding,
    grayscale
) where
import Data.Vector as DV
import PPM

-- helper function to extract elements from RGB
fst' :: RGB -> Int 
fst' (RGB (a,_,_)) = a
fst'' :: RGB -> Double 
fst'' (RGB (a,_,_)) = fromIntegral a
snd' :: RGB -> Int 
snd' (RGB (_,a,_)) = a
trd' :: RGB -> Int
trd' (RGB (_,_,a)) = a



{--------------------grayscale--------------------}

-- helper function for grayscale 
average' :: RGB -> RGB
average' x = RGB(avg, avg, avg) 
            where avg = div (fst' x + snd' x + trd' x) 3 
grayscale :: PPMImage RGB -> PPMImage RGB
grayscale ppm = fmap average' ppm
abs' :: Double -> Double -> Double
abs' x y = if ( x > y ) then ( x - y ) else (y - x)


{------------------autoThresholding----------------}

{--
    helper function to autoThresholding
    it calculates the threshold value that we need to distinguish white and black
--}
thresholdHelper :: Double -> Double -> PPMImage RGB -> Double
thresholdHelper limit t grayimage = 
    if ( (abs' t newT) <= limit ) then (t + newT) / 2 else thresholdHelper limit newT grayimage -- if less than limit, stop, otherwise continue recursion
    where
        pixelvalues = pixels grayimage
        greaterNum = DV.length (DV.filter (\x -> fst'' x > t) pixelvalues) 
        lessNum = DV.length pixelvalues - greaterNum
        greaterSum = DV.foldl(\y x -> if fst'' x > t then (y + fst'' x) else y) 0 pixelvalues
        lessSum = DV.foldl(\y x -> if fst'' x <= t then (y + fst'' x) else y) 0 pixelvalues
        greaterAvg = greaterSum / fromIntegral greaterNum
        lessAvg = lessSum / fromIntegral lessNum
        newT = (greaterAvg + lessAvg) / 2     
-- categorize pixels into black and white according to thresholdHelper's return
autoThresholding :: Int -> PPMImage RGB -> PPMImage RGB
autoThresholding limit ppm = fmap (\x -> if (fst'' x >= t) then RGB(255,255,255) else RGB(0,0,0)) grayimage
    where
        grayimage = grayscale ppm
        pixelvalues = pixels grayimage
        t = thresholdHelper (fromIntegral limit) (fromIntegral 127) grayimage


{------------------------blend---------------------------}

--helper functions to blend
--maxAdd limit the upperbound of samples
maxAdd :: Int -> Int -> Int
maxAdd a b
    | a + b > 255 = 255
    | otherwise = a + b
--addRGB add 2 rgb together
addRGB :: RGB -> RGB -> RGB
addRGB (RGB (a0,b0,c0)) (RGB (a1,b1,c1)) = RGB (maxAdd a0 a1, maxAdd b0 b1, maxAdd c0 c1)
--using Applicative to apply addRGB function on ppm
blendHelper :: PPMImage RGB -> PPMImage RGB -> PPMImage RGB 
blendHelper x y = PPMImage 10 10 255 (DV.replicate (DV.length (pixels x)) addRGB) <*> x <*> y 

blend :: [PPMImage RGB]  -> PPMImage RGB
blend [] = PPMImage 10 10 255 (DV.replicate 0 $ RGB (0,0,0)) --default ppm, which is all black
blend lst = Prelude.foldl (\x y -> blendHelper x y) (PPMImage 10 10 255 zero) lst --blend each ppm together
        where zero = DV.replicate (DV.length (pixels (lst!!0))) $ RGB (0,0,0)

{------------------------findContours------------------------}

type Contour = [(Int,Int)]
findContours :: PPMImage RGB -> ([Contour],PPMImage RGB)
findContours ppm = (clst, PPMImage w h 255 res)
    where
        w = width ppm
        h = height ppm
        thresHoldPPM = autoThresholding 0 ppm
        len = DV.length (pixels thresHoldPPM)
        clst = loopTheImage thresHoldPPM (DV.replicate len (-1)) 0  --contour list
        black = DV.replicate len (RGB (0, 0, 0)) -- fill the outppm to be entire black first
        res = Prelude.foldl (\x y -> (Prelude.foldl (\a b -> a DV.// [((snd b) * w + (fst b), RGB (255, 255, 255)) ]) x y)) black clst

--Below are helper functions of findContours

{--
    I assume anticlockwise direction representation. (0 - up, 1 - left, 2 - down, 3 - right)
    Also, I assume if touching the edge, it only turn direction and remain at the same position(details in README.md (Edgecase handling)).
--}
newposHelper :: Int -> Int -> Int -> (Int, Int) -> Int -> (Int, Int, Int)
newposHelper leftOrRight width height (x, y) dir 
    | dir == 0 = if (y - 1) < 0 then newpos else (x, y - 1, 0)
    | dir == 1 = if (x - 1) < 0 then newpos else (x - 1, y, 1)
    | dir == 2 = if (y + 1) >= height then newpos else (x, y + 1, 2)
    | otherwise = if (x + 1) >= width then newpos else (x + 1, y, 3)
    where newpos = newposHelper leftOrRight width height (x,y) (rem (dir + leftOrRight) 4) -- change direction

-- return index according to position
idxHelper :: Int -> Int -> Int -> Int
idxHelper width x y = y * width + x

{-- 
    Do the square tracing algorithm.
    dirvec is a vector of Int which only composes of -1, 0, 1, 2, 3, which records the "first-time entering" direction of each position, where -1 means haven't been entered.
    See details in README.md Direction recorded Vector.
--}
squareTracing :: PPMImage RGB -> DV.Vector Int -> Contour -> (Int, Int, Int) -> (Contour, DV.Vector Int)
squareTracing ppm dirvec contour curpos@(x,y,dir) 
    | dir == dirvec DV.! idx = (contour, dirvec) 
    | isBlack (rgbvec DV.! idx) = squareTracing ppm newdirvec newcontour newleftpos
    | otherwise = squareTracing ppm newdirvec contour newrightpos
    where 
        rgbvec = pixels ppm
        idx = idxHelper (width ppm) x y
        newcontour = if (dirvec DV.! idx) == -1 && (isValidStart ppm idx) /= -1 then (x,y) : contour else contour
        newdirvec = if (dirvec DV.! idx) == -1 then dirvec DV.// [(idx, dir)] else dirvec
        newleftpos@(lx, ly, ldir) = newposHelper 1 (width ppm) (height ppm) (x,y) dir
        newrightpos@(rx, ry, rdir) = newposHelper 3 (width ppm) (height ppm) (x,y) dir

-- helper function to check whether rgb is black or not
isBlack :: RGB -> Bool
isBlack (RGB (0, 0, 0)) = True
isBlack _ = False
         
{--
    helper function to check whether a position is all surronded by black pixels.
    Actually, it is not only been applied to check whether the position is a valid start or not, 
    it is also been applied to check whether the position is eligible to be added to contour.
    The return of this function only has 5 values, -1, 0, 1, 2, 3, where -1 means it is not a valid start, 
    and other values indicate the correct direction for this position to start. 
    However, when using this function in squareTracing(line 126), it only checks whether the return is not -1 or not,
    so the direction doesn't matter in this case, it is just used as an indicator to determine whether to add it into the contour or not.
--}
isValidStart :: PPMImage RGB -> Int -> Int
isValidStart ppm idx = allBlack
    where
        w = width ppm
        h = height ppm
        rgblst = pixels ppm
        (x, y) = (rem idx w, div idx w)    
        neighbours = Prelude.filter (\a -> (w * a!!1 + a!!0) >= 0 && (w * a!!1 + a!!0) < w*h) [[x, y-1, 0], [x-1, y, 1], [x, y+1, 2], [x+1, y, 3]]    
        allBlack = Prelude.foldl(\x y -> if (isBlack (rgblst DV.! (y!!1 * w + y!!0))) then x else y!!2)  (-1) neighbours
 
{--
    helper function to loop the entire pixels.
    in line 162, it means, if the position is black, and it is not all surrounded by black pixels(isValidStart in line 168),
    and whether the position has not been entered before or its initial enter position is not the same as the direction it will use to enter for this time(dirvec DV.! idx /= dir),
    then call squareTracing at this position and add the new contour if it is not empty.
--}
loopTheImage :: PPMImage RGB -> DV.Vector Int -> Int -> [Contour]
loopTheImage ppm dirvec idx
    | idx == DV.length dirvec = [] -- reach end, terminate
    | isBlack (rgb DV.! idx) && dir /= -1 && (dirvec DV.! idx /= dir) = if Prelude.length newc /= 0 then newc : newloop else newloop
    | otherwise = loopTheImage ppm dirvec (idx + 1) --go to next index
    where
        w = width ppm
        h = height ppm
        rgb = pixels ppm
        dir = isValidStart ppm idx
        pos = (rem idx w, div idx w, dir)        
        (newc, newdirvec) = squareTracing ppm dirvec [] pos 
        newloop = loopTheImage ppm newdirvec (idx + 1)