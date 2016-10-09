{-# LANGUAGE ExistentialQuantification #-}
module Main where

import Debug.Trace
import Termin
import Geom
import Data.Maybe
import System.Random
import Control.Monad
import Control.Monad.Trans.State

main :: IO ()
main = startGame (toGame action) newWorld

newWorld :: GameAux -> Game World
newWorld aux = Game newAux $ World train [] 0 0
    where (train, newAux) = runState mkTrain aux

-- I don't actually need aux for the action here
toGame :: (World -> Char -> World) -> (Game World -> Char -> Game World)
toGame action = (\(Game aux world) ch -> Game aux $ action world ch)

action :: World -> Char -> World
action world@(World train msg attempts moves) ch = fromMaybe world newWorld
    where newWorld = fmap (\dir -> World (moveTrain train dir) msg attempts $ moves+1) (getDir ch)


moveTrain :: Train -> Dir -> Train
moveTrain (Train size car) LeftD = Train size $ nextNode car
moveTrain (Train size car) RightD = Train size $ prevNode car
moveTrain train _ = train

getDir :: Char -> Maybe Dir
getDir 'D' = Just LeftD
getDir 'A' = Just UpD
getDir 'C' = Just RightD
getDir 'B' = Just DownD
getDir _   = Nothing

data World = World {train :: Train, msg :: [Title], attempts :: Int, moves :: Int}
instance Renderable World where
    render (World train msg attempts moves) = concat $ map render (RS train:(map RS msg))

data Renderables = forall a. Renderable a => RS a
instance Renderable Renderables where
    render (RS a) = render a

data Train = Train {size :: Int, car :: DList Bool}

instance Show Train where
    show (Train size car) = show $ takeF size car

instance Renderable Train where
    render train | isLightOn train = render carLight
                 | otherwise       = render carNoLight

isLightOn :: Train -> Bool
isLightOn (Train size (DLNode prev light next)) = light

-- inspired by the steam locomotive: $ strings `which sl`
carLight :: [String]
carLight = [" ________________________ ",
            " |  ____ ____ ____ ____ | ",
            "=|  |██| |☻ | |██| |██| |=",
            " |______________________| ",
            "    (O)           (O)     "]

carNoLight :: [String]
carNoLight = [" ________________________ ",
              " |  ____ ____ ____ ____ | ",
              "=|  |__| |☺ | |__| |__| |=",
              " |______________________| ",
              "    (O)           (O)     "]


mkTrain :: AuxState Train
mkTrain = genTrainSize >>= \size -> genLights size >>= \lights -> return $ Train size $ mkDList lights
    where genLights n = replicateM n $ genRand random
          genTrainSize = genRand $ randomR (5, 20) --good luck solving it in ^2 time

-- some kind of State StdGen to State GameAux transformer, there is probably some built in thing I'm missing...
genRand :: (StdGen -> (a, StdGen)) -> AuxState a
genRand f = state (\(GameAux wnd rnd) -> (\(x, rnd') -> (x, GameAux wnd rnd')) $ f rnd)

-- rebuild the whole thing, see http://xkcd.com/1737/
switchLight :: Train -> Train
switchLight (Train size (DLNode prev light next)) = Train size $ mkDList $ not light : (takeF (size-1) next)

-- wiki.haskell.org/Tying_the_Knot
data DList a = DLNode {prevNode :: DList a, el :: a, nextNode:: DList a}

mkDList :: [a] -> DList a
mkDList [] = error "must have at least one element"
mkDList xs = let (first,last) = go last xs first
             in trace "mkDList" first

  where go :: DList a -> [a] -> DList a -> (DList a, DList a)
        go prev []     next = (next,prev)
        go prev (x:xs) next = let this        = DLNode prev x rest
                                  (rest,last) = go this xs next
                              in  (this,last)

takeF :: Int -> DList a -> [a]
takeF 0     _                 = []
takeF n (DLNode _ x next) = x : (takeF (n-1) next)

takeR :: Show a => Int -> DList a -> [a]
takeR 0     _                 = []
takeR n (DLNode prev x _) = x : (takeR (n-1) prev)