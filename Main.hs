{- 
  File      :  Main.hs 
  Copyright : (c) Hanwen Xu, 03/06/19 
-}
module Main
(
    main 
)
where 

import System.IO  
import ImageProc as IP
import PPM
import System.Environment
import Data.List as DL
import Data.Vector as DV
import Control.Monad as CM
usage :: String
usage = 
    "usage :imageproc <PPM in file path> <PPM out file path> <proc>\n" Prelude.++
    "  <proc>:\n" Prelude.++ 
    "       contour\n" Prelude.++
                -- Finds the contours of the in
    "                 file path. Prints the list of\n" Prelude.++
    "                 contours to the screen\n" Prelude.++
    "       blend <PPM file1> ... <PPM fileN>:\n" Prelude.++
               -- Blends the provided images
    "                 with the <PPM in file path> image\n" Prelude.++
    "                 and saves it to <PPM out file path>\n" Prelude.++
    "       autoThreshold <LIMIT>\n" Prelude.++ 
               -- Performs an auto thresholding effect
    "                 on the <PPM in file path> image and\n" Prelude.++
    "                 saves it to <PPM out in file path>\n" Prelude.++
    "                 Limit is the max number of iterations\n" Prelude.++
    "                 during auto-thresholding.\n" Prelude.++
    "       grayscale\n" Prelude.++ 
                -- Performs a grayscale effect on the
    "                 <PPM in file path> and saves it to\n" Prelude.++
    "                 <PPM out file path>\n"

-- helper functions for RGB sample extraction
fst' :: RGB -> Int 
fst' (RGB (a,_,_)) = a
snd' :: RGB -> Int 
snd' (RGB (_,a,_)) = a
trd' :: RGB -> Int
trd' (RGB (_,_,a)) = a

-- valiation the ppm file
validation :: String -> Bool
validation s
    | len < 4 = False --no body
    | Prelude.length (words (parsedString!!0)) /= 1 = False --more than one magic number
    | Prelude.length dim /= 2 = False --other than 2 dimensions
    | Prelude.length (words (parsedString!!2)) /= 1 = False --more than one maxColor
    | Prelude.length body /= 3 * dim!!0 * dim!!1 = False--wrong total number of samples
    | otherwise = Prelude.foldl(\x y -> if (y < 0 || y > maxc ) then False else x) True body --color out of range
    where
        parsedString = lines s
        body = Prelude.map(\x -> read x::Int) (Prelude.drop 4 (words s))
        maxc = read (parsedString !! 2)::Int
        len = Prelude.length parsedString
        dim = Prelude.map (\x -> read x::Int) (words (parsedString!!1))

--helper function for parsePPM
splitHelper :: [String] -> DV.Vector RGB
splitHelper [] = empty
splitHelper lst = cons (RGB (read (lst!!0)::Int, read (lst!!1)::Int, read (lst!!2)::Int)) (splitHelper (Prelude.drop 3 lst))
--parse string to a ppm
parsePPM :: String -> PPMImage RGB
parsePPM s = PPMImage w h maxC (splitHelper body)
    where
        parsedString = lines s
        body = Prelude.drop 4 (words s)
        dim = words (parsedString!!1)
        w = read (dim!!0)::Int
        h = read (dim!!1)::Int
        maxC = read (parsedString!!2) ::Int
        len = Prelude.length parsedString

--convert PPMImage to a formatted string
writeHelper :: DV.Vector RGB -> String
writeHelper rgb = DV.foldl (\x y -> x Prelude.++ show (fst' y) Prelude.++ " " Prelude.++ show (snd' y) Prelude.++ " " Prelude.++ show (trd' y) Prelude.++ "\n") "" rgb
writePPM :: PPMImage RGB-> String
writePPM ppm = "P3\n" Prelude.++  show (width ppm) Prelude.++ " " Prelude.++ show (height ppm)
                Prelude.++ "\n" Prelude.++ show (maxColor ppm) Prelude.++ "\n" Prelude.++ (writeHelper (pixels ppm))

--helper function to main
mainHelper :: [String] -> IO()
mainHelper (filein:fileout:"grayscale":[]) = do
    contentin <- readFile filein
    let ppm = parsePPM contentin
    let outPPM = grayscale ppm
    writeFile fileout (writePPM outPPM)
mainHelper (filein:fileout:"autoThreshold":rest) = do
    contentin <- readFile filein
    let ppm = parsePPM contentin
    let limit = read (rest!!0)::Int
    let outPPM = autoThresholding limit ppm
    writeFile fileout (writePPM outPPM)
mainHelper (filein:fileout:"blend":rest) = do
    contentin <- readFile filein
    restlst <- Prelude.mapM readFile rest
    let valid = Prelude.foldl (\x y -> if ( snd x /= -1 ) then (fst x + 1, snd x)
                                        else if (( validation y) == False) then (fst x + 1, fst x)
                                        else (fst x + 1, snd x) ) (0, -1) restlst
    if ( snd valid /= -1 ) then do
        putStrLn $ "Invalid PPM image: " Prelude.++  rest !! (snd valid)

    else do
        let ppmlst = Prelude.map parsePPM (contentin : restlst)
        let outPPM = blend ppmlst
        writeFile fileout (writePPM outPPM)
mainHelper (filein:fileout:"contour":[]) = do
    contentin <- readFile filein
    let ppm = parsePPM contentin
    let outPPM = snd (findContours ppm)
    writeFile fileout (writePPM outPPM)
mainHelper _ = return ()

main :: IO()
main = do
    args <- getArgs
    case args of
        filein:fileout:proc:rest -> do
            contentin <- readFile filein
            if (validation contentin) == False then do
                putStrLn $ "Invalid PPM image: " Prelude.++ filein
            else
                mainHelper (filein:fileout:proc:rest)
        _ -> putStrLn usage 


            




