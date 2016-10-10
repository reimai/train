{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Termin
import Geom
import Train
import Data.Maybe
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
          newMoves = if (cmd == Just (Move LeftD) || cmd == Just (Move RightD)) then moves + 1 else moves

animate :: World -> Command -> World -> [[Point]]
animate before(Move dir) after@(World _ msg att moves)  = map frameInWorld $ animateTrains (train before) dir (train after)
    where frameInWorld t = concatMap (render.framed) (RS t:(map RS msg) ++ (map RS $ ["moves: " ++ (show moves)]))
animate _ _ _ = []

actTrain :: Train -> Command -> Train
actTrain train SwitchLight = switchLight train
actTrain train (Move dir) = moveInTrain train dir

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
    render (World train msg attempts moves) = concatMap (render.framed) (RS train:(map RS msg) ++ (map RS $ ["moves: " ++ (show moves)]))


data Renderables = forall a. Renderable a => RS a
instance Renderable Renderables where
    render (RS a) = render a

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