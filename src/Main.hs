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
newWorld aux = Game newAux $ World train "" 0 0 False
    where (train, newAux) = runState mkTrain aux

-- I don't actually need aux for the action here
toGame :: (World -> String -> ([[Point]], World)) -> Action World [Point]
toGame action = \(Game aux world) str ->
                    let (animation, newWorld) = action world str in
                    (animation, Game aux $ newWorld)

action :: World -> String -> ([[Point]], World)
action world str = fromMaybe ([], world) updated
    where updated = fmap (\c -> let newWorld = actWorld world c in
                        (animate world c newWorld, newWorld)) $ parseCommand str

actWorld :: World -> Command -> World
actWorld w@(World _ _ _ _ True) _ = w
actWorld world (LetMeOut guess) = goblinKing world guess
actWorld world cmd = world {train = actTrain oldTrain cmd, moves = newMoves, msg = ""}
    where oldMoves = moves world
          oldTrain = train world
          newMoves = if (cmd == Move LeftD || cmd == Move RightD) then oldMoves + 1 else oldMoves

goblinKing :: World -> Int -> World
goblinKing world guess | guess == (size $ train world) = world {won = True}
                       | otherwise = world {attempts = (attempts world) + 1}

animate :: World -> Command -> World -> [[Point]]
animate before(Move dir) after = map (renderInWorld after) $ animateTrains (train before) dir (train after)
animate _ _ _ = []

actTrain :: Train -> Command -> Train
actTrain train SwitchLight = switchLight train
actTrain train (Move dir) = moveInTrain train dir

data Command = Move Dir | LetMeOut Int | SwitchLight deriving (Show, Eq)

parseCommand :: String -> Maybe Command
parseCommand " " = Just SwitchLight
parseCommand str = orElse (fmap Move $ parseDir str) (fmap LetMeOut $ parseInt str)

orElse :: Maybe a -> Maybe a -> Maybe a
orElse x@(Just a) _ = x
orElse _ y = y

parseInt :: String -> Maybe Int
parseInt str | head str == '\n' = readMaybe str
                  | otherwise = Nothing

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
                  [(val, "")] -> Just val
                  _           -> Nothing

parseDir :: String -> Maybe Dir
parseDir "D" = Just LeftD
parseDir "A" = Just UpD
parseDir "C" = Just RightD
parseDir "B" = Just DownD
parseDir _   = Nothing

data World = World {train :: Train, msg :: String, attempts :: Int, moves :: Int, won :: Bool}
instance Renderable World where
    render world = renderInWorld world $ train world

renderInWorld :: Renderable a => World -> a -> [Point]
renderInWorld (World _ _ _ _ True) _ = render.framed $ center "You freaking won!" (28, 16)
renderInWorld (World _ msg attempts moves won) r = concatMap (render.framed) $
                                                RS r : RS (Title msg (Crd 0 11)) : RS ("moves: " ++ (show moves)) : []

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