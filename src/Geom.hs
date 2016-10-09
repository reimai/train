{-# LANGUAGE FlexibleInstances #-}

module Geom (
Title(..), Dir(..), Crd(..), Point(..),
Renderable, render,
move, center, isOpposite, isX, isY
) where


move :: Crd -> Dir -> Int -> Crd
move (Crd x y) dir n | isX dir   = Crd (x+nSign) y
                     | otherwise = Crd x (y+nSign)
    where nSign | isPositive dir = n
                | otherwise = -n

data Crd = Crd {x :: Int, y :: Int} deriving (Eq, Show)

data Point = Point {crd :: Crd, sym :: Char} deriving (Eq, Show)
instance Renderable Point where
    render p = [p]

movePoint :: Dir -> Int -> Point -> Point
movePoint dir n (Point crd sym) = Point (move crd dir n) sym

class Renderable a where
    render :: a -> [Point]

data Title = Title {str :: String, pos :: Crd}
    deriving (Eq, Show)

instance Renderable Title
    where render (Title str (Crd x y)) = map ((movePoint DownD y).(movePoint RightD x)) $ render str

instance Renderable [String]
    where render strs = concatMap (\(i, ps) -> map (movePoint DownD i) ps) $ zip [0..] $ map render strs

instance Renderable String
    where render str = map (\(i, s) -> Point (Crd (i) 0) s) $ zip [0..] str

data Dir = RightD | LeftD | UpD | DownD
    deriving (Eq, Show)

center :: String -> (Int, Int) -> Title
center str (w, h) = Title str $ Crd ((w - (length str)) `div` 2) (h `div` 2)

isOpposite :: Dir -> Dir -> Bool
isOpposite a b = (a /= b) && (isX a) == (isX b)

isX :: Dir -> Bool
isX LeftD = True
isX RightD = True
isX _ = False

isY :: Dir -> Bool
isY = not.isX

isPositive :: Dir -> Bool
isPositive RightD = True
isPositive DownD = True
isPositive _ = False