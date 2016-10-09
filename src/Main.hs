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
toGame :: (World -> Char -> ([[Point]], World)) -> Action World [Point]
toGame action = \(Game aux world) ch ->
                    let (animation, newWorld) = action world ch in
                    (animation, Game aux $ newWorld)


action :: World -> Char -> ([[Point]], World)
action world@(World train msg attempts moves) ch = fromMaybe ([], world) updated
    where updated = fmap (\cmd -> (animate world cmd (newWorld cmd), newWorld cmd)) cmd
          newWorld dir = World (actTrain train dir) msg attempts newMoves
          cmd = parseCommand ch
          newMoves = fromMaybe moves $ fmap (\c -> if c == SwitchLight then moves else moves + 1) cmd

animate :: World -> Command -> World -> [[Point]]
animate before (Move dir) after = animateTrains (train before) dir (train after)
animate _ _ _= []

animateTrains :: Train -> Dir -> Train -> [[Point]]
animateTrains before dir after = map (framed.Moved 0 4) $ sliding (showTrain before) dir (showTrain after)

sliding :: [String] -> Dir -> [String] -> [[String]]
sliding before RightD after = reverse $ keepSliding [] before after
sliding before LeftD after = keepSliding [] after before
sliding before _ after = []

keepSliding :: [[String]] -> [String] -> [String] -> [[String]]
keepSliding acc b a = if (all null a) then b:acc else keepSliding (res : acc) res undone
    where (res, undone) = slide b a

slide :: [String] -> [String] -> ([String], [String])
slide fs ts = (map (\(f,t) -> tail f ++ [head t]) (zip fs ts), map tail ts)

actTrain :: Train -> Command  -> Train
actTrain train SwitchLight = switchLight train
actTrain (Train size car) (Move LeftD) = Train size $ nextNode car
actTrain (Train size car) (Move RightD) = Train size $ prevNode car
actTrain train _ = train

data Command = Move Dir | SwitchLight deriving (Show, Eq)

parseCommand :: Char -> Maybe Command
parseCommand ' ' = Just SwitchLight
parseCommand ch = fmap Move $ parseDir ch

parseDir :: Char -> Maybe Dir
parseDir 'D' = Just LeftD
parseDir 'A' = Just UpD
parseDir 'C' = Just RightD
parseDir 'B' = Just DownD
parseDir _   = Nothing

data World = World {train :: Train, msg :: [Title], attempts :: Int, moves :: Int}
instance Renderable World where
    render (World train msg attempts moves) = concat $ map render (RS train:(map RS $ map framed msg) ++ (map RS $ map framed $ ["moves: " ++ (show moves)]))

data Renderables = forall a. Renderable a => RS a
instance Renderable Renderables where
    render (RS a) = render a

data Train = Train {size :: Int, car :: DList Bool}

instance Renderable Train where
    render train = framed $ Moved 0 4 $ showTrain train

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

framed :: Renderable a => a -> [Point]
framed smth = (render theFrame) ++ (render $ Moved 10 6 smth)

theFrame :: Moved [String]
theFrame = Moved 9 5 $ frame 28 20

frame :: Int -> Int -> [String]
frame w h | w <= 2 || h <= 2 = error "that's a wee frame"
          | otherwise = top : (replicate (h-2) body) ++ [bottom]
                where top   = ('╔' : replicate (w-2) '═' ++ "╗")
                      body   = '║' : replicate (w-2) ' ' ++ "║"
                      bottom =('╚' : replicate (w-2) '═' ++ "╝")

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