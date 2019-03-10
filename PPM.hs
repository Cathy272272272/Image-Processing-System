{- 
  File      :  PPM.hs 
  Copyright : (c) Hanwen Xu, 03/06/19 
-}
module PPM (
    PPMImage(..),
    RGB(..),
) 
where
import Data.Vector as DV
import Control.Applicative
import qualified Data.Foldable as F
data PPMImage a = PPMImage { 
    width :: Int,
    height:: Int,
    maxColor :: Int, -- Not really required since 255 is t -- max color for this assignment.
    pixels :: DV.Vector a
}deriving(Show)
newtype RGB = RGB (Int,Int,Int) deriving(Show)

-- loop is a helper function for Applicative, it applies each functor on each rgb according to their indexes
loop :: Int -> DV.Vector (a -> b) -> DV.Vector a -> DV.Vector b
loop idx fs xs
    | (idx == DV.length fs || idx == DV.length xs ) = DV.fromList []
    | otherwise = cons ( (fs DV.! idx) (xs DV.! idx) ) (loop (idx + 1) fs xs)

instance Functor PPMImage where
    fmap f (PPMImage w h m p) = PPMImage w h m (DV.map f p)

instance Applicative PPMImage where
    pure a = PPMImage 10 10 255 (DV.fromList [a])
    a <*> b
        | (width a /= 10 || width b /= 10 || height a /= 10 || height b /= 10 ) = error "PPMImage must be a 10x10 image"
        | otherwise = PPMImage 10 10 255 (loop 0 (pixels a) (pixels b))

instance F.Foldable PPMImage where
    foldr f a b = F.foldr f a (pixels b)