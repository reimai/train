--module for stupid & simple console games
module Termin (
    addToScreen, readAll, startGame, normalize, Game(..), GameAux(..), Action(..), AuxState, getCrd
) where

import Data.List
import System.Timeout
import System.Console.ANSI
import qualified System.Console.Terminal.Size as T
import System.Console.Terminal.Size (Window)
import Data.Maybe
import qualified Data.Text.IO as TextIO
import qualified Data.Text as Text
import Control.Monad
import Control.Concurrent
import System.IO
import Control.Concurrent
import System.CPUTime
import Geom
import System.Random
import Control.Monad.Trans.State

data Renderable a => Game a = Game{aux :: GameAux, world :: a}

data GameAux = GameAux{wnd :: (Int, Int), rnd :: StdGen}

type AuxState a = State GameAux a
--action updates the world and may return some animaion as a side effect
type Action a b = Game a -> Char -> ([b], Game a)

getX :: AuxState Int
getX = getOneCrd fst

getY :: AuxState Int
getY = getOneCrd snd

getOneCrd :: ((Int,Int) -> Int) -> AuxState Int
getOneCrd chooseCrd = state (\(GameAux wnd rnd) -> (\(i, rnd') -> (i, GameAux wnd rnd')) $ randomR (0, (chooseCrd wnd)) rnd)

getCrd :: AuxState Crd
getCrd = getX >>= \x -> getY >>= \y -> return $ Crd x y


startGame :: (Renderable a, Renderable b) => Action a b -> (GameAux -> Game a) -> IO()
startGame action initWorld = do
        hSetBuffering stdin NoBuffering --get input immediately
        hSetEcho stdin False            --don't show the typed character
        jwindow <- T.size
        let wnd = fromJust jwindow
        rnd <- getStdGen
        let w = T.width wnd
        let h = T.height wnd - 1
        gameLoop stdin (initWorld $ GameAux (w, h) rnd) action


--main loop, read input, change the world, redraw screen 
gameLoop :: (Renderable a, Renderable b) => Handle -> Game a -> Action a b -> IO()
gameLoop input game@(Game aux _) action = do
                        animate game
                        ch <- readAll input ' '
                        let (animation, newWorld) = action game ch
                        when (ch /= 'q') $ mapM_ animate (map (\w -> Game aux w) animation) >> gameLoop input newWorld action


animate :: Renderable a => Game a -> IO()
animate game@(Game (GameAux (w, h) rnd) world) = do
            mapM_ putStrLn $ addToScreen (replicate h $ replicate w ' ') (w,h) world
            e <- threadDelay (floor(1/fps * 10^6))
            return ()
                where fps = 15


addToScreen :: Renderable a => [String] -> (Int, Int) -> a -> [String]
addToScreen ls wnd obj = map (\(i, l, ps) -> mergeLine l ps) $ groupByLines ls $ filter (\p -> fits wnd $ crd p) $ render obj  

fits :: (Int,Int) -> Crd -> Bool
fits (w,h) (Crd x y) = x < w && x >= 0 && y >= 0 && y < h                             

normalize :: (Int, Int) -> Crd -> Crd
normalize (w, h) (Crd x y) = Crd (x `mod` w) (y `mod` h) 

mergeLine :: String -> [Point] -> String
mergeLine line ps = mergeLine' (zip [0..] line) $ sortOn (x.crd) ps 

mergeLine' :: [(Int, Char)] -> [Point] -> String
mergeLine' l [] = map snd l 
mergeLine' [] p = error $ "no cols " ++ (show p)
mergeLine' lines@((il, l):ls) (p:ps) | il <  (x.crd $ p) = l:(mergeLine' ls (p:ps))
                               | il == (x.crd $ p) = (sym p):(mergeLine' ls ps)
                               | otherwise         = mergeLine' lines ps --duplicate crd


groupByLines  :: [String] -> [Point] -> [(Int, String, [Point])]
groupByLines ls ps = mergeToLines (zip3 [0..] ls $ repeat []) $ map (\p -> (y.crd $ p, p)) $ sortOn (y.crd) ps

mergeToLines :: (Ord a, Show a, Show c) => [(a, b, [c])] -> [(a, c)] -> [(a, b, [c])]
mergeToLines x [] = x
mergeToLines [] y = error $ "no lines" ++ (show y)
mergeToLines ((x, xd, xyd):xs) ((y, yd):ys) | x < y     = (x, xd, xyd):(mergeToLines xs ((y, yd):ys))
                                            | otherwise = mergeToLines ((x, xd, yd:xyd):xs) ys 
               
--read all input from Handle, return only the last char or default
readAll :: Handle -> Char -> IO(Char)
readAll h defaultCh = hReady h >>= \gotIt -> if gotIt then (hGetChar h >>= readAll h) else return defaultCh
