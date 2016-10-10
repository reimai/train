module Train(
Train(..), showTrain, mkTrain, switchLight, moveInTrain, animateTrains
) where

import Geom
import Termin
import System.Random
import Control.Monad
import Control.Monad.Trans.State

data Train = Train {size :: Int, car :: DList Bool}

instance Renderable Train where
    render train = render $ Moved 0 4 $ showTrain train

animateTrains :: Train -> Dir -> Train -> [[Point]]
animateTrains before dir after = map (render.Moved 0 4) $ sliding (showTrain before) dir (showTrain after)

showTrain :: Train -> [String]
showTrain car = if (isLightOn car) then carLight else carNoLight

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
          genTrainSize = genRand $ randomR (5, 30) --good luck solving it in ^2 time

-- some kind of State StdGen to State GameAux transformer, there is probably some built in thing I'm missing...
genRand :: (StdGen -> (a, StdGen)) -> AuxState a
genRand f = state (\(GameAux wnd rnd) -> (\(x, rnd') -> (x, GameAux wnd rnd')) $ f rnd)

moveInTrain :: Train -> Dir -> Train
moveInTrain train@(Train size car) dir | dir == RightD = Train size $ nextNode car
                                       | dir == LeftD = Train size $ prevNode car
                                       | otherwise = train

-- rebuild the whole thing, see http://xkcd.com/1737/
switchLight :: Train -> Train
switchLight (Train size (DLNode prev light next)) = Train size $ mkDList $ not light : (takeF (size-1) next)

-- wiki.haskell.org/Tying_the_Knot
data DList a = DLNode {prevNode :: DList a, el :: a, nextNode:: DList a}

mkDList :: [a] -> DList a
mkDList [] = error "must have at least one element"
mkDList xs = let (first,last) = go last xs first
             in first

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