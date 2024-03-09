{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Monad ((<=<))
import Control.Monad.Fix (fix)
import Data.Map (Map)
import qualified Data.Map as Map
import Graphics.Vty
    ( Event (..)
    , Image
    , Key (..)
    , Modifier (..)
    , Output (displayBounds)
    , Vty (..)
    , defAttr
    , defaultConfig
    , green
    , nextEvent
    , picForImage
    , shutdown
    , string
    , update
    , vertCat
    , withForeColor
    , (<->)
    )
import Graphics.Vty.CrossPlatform (mkVty)
import System.Environment (getArgs)
import System.IO
    ( Handle
    , IOMode (ReadMode)
    , hGetLine
    , hGetPosn
    , hIsEOF
    , hSetPosn
    , withFile
    )

------------------------- State -------------------------

newtype Line = Line Int
    deriving (Show, Eq, Ord, Num, Enum)

data State = State
    { cursor :: Line -- the first line number we are showing
    , height :: Line -- the number of lines we are showing
    , exit :: Bool -- if we should exit
    }

updateCursor :: (Line -> Line -> Line) -> State -> State
updateCursor f (State c h e) = State (f c h) h e

updateHeight :: Line -> State -> State
updateHeight h (State c _ e) = State c h e

updateExit :: Bool -> State -> State
updateExit e (State c h _) = State c h e

boot
    :: Line -- the number of lines we are showing at boot time
    -> State
boot h = State 0 h False

renderBar :: BarHeight -> State -> Image
renderBar (BarHeight b) (State (Line c) (Line h) _) =
    let
        f =
            string (defAttr `withForeColor` green)
                $ "Cursor: " ++ show c ++ " Height: " ++ show (h - b)
        g = string defAttr ""
     in
        vertCat [f, g]

------------------------- Page -------------------------
data Page = Page
    { pageLine :: Line -- the first line number the page is showing
    , pageHeight :: Line -- the number of lines the page is showing
    }

newtype BarHeight = BarHeight Int

viewPage :: BarHeight -> State -> Page
viewPage (BarHeight b) (State c h _) = Page c (h - Line b)

newtype PageContent = PageContent [String]

renderPage :: PageContent -> Image
renderPage (PageContent ps) = vertCat $ string defAttr <$> ps

------------------------- State Transition -------------------------

data Action = Next | Prev | Resize Line | Quit

changeState :: BarHeight -> Line -> Action -> State -> State
changeState (BarHeight b) top Next = updateCursor
    $ \n h -> if n + h - Line b > top then n else n + h - Line b
changeState (BarHeight b) _ Prev = updateCursor $ \n h -> max 0 $ n - h + Line b
changeState _ _ Quit = updateExit $ True
changeState _ _ (Resize h) = updateHeight h

eventToAction :: Event -> Maybe Action
eventToAction (EvKey KPageUp []) = Just Prev
eventToAction (EvKey KPageDown []) = Just Next
eventToAction (EvKey KDown []) = Just Next
eventToAction (EvKey KUp []) = Just Prev
eventToAction (EvKey (KChar 'q') []) = Just Quit
eventToAction (EvKey (KChar 'c') [MCtrl]) = Just Quit
eventToAction (EvResize _ h) = Just $ Resize $ Line h
eventToAction _ = Nothing
-- eventToAction x = error $ "Unknown event: " ++ show x

------------------------- File Operation -------------------------

getLineN :: Content -> Line -> IO String
getLineN m n = case Map.lookup n m of
    Nothing -> pure ""
    Just pos -> pos

getLines :: Content -> [Line] -> IO [String]
getLines fl = mapM $ getLineN fl

getPage :: Content -> Page -> IO PageContent
getPage fl (Page n m) =
    PageContent <$> getLines fl [n .. n + m - 1]

------------------------- Content handling -------------------------
-- actions to get specific lines from a file
type Content = Map Line (IO String)

-- return the action to read the line at the current handle position
getContentLine :: Handle -> IO (IO String)
getContentLine handle = do
    pos <- hGetPosn handle
    pure $ do
        hSetPosn pos
        l <- hGetLine handle
        pure $ filter (/= '\r') l

-- run a function against the content of a file
withContent :: FilePath -> (Content -> IO a) -> IO a
withContent fp action = do
    withFile fp ReadMode $ \handle -> do
        let go lineNumber content = do
                eof <- hIsEOF handle
                if eof
                    then action content
                    else do
                        contentLine <- getContentLine handle
                        _ <- hGetLine handle
                        go (lineNumber + 1)
                            $ Map.insert lineNumber contentLine content
        go 0 Map.empty

------------------------- Main Loop -------------------------

barHeight :: BarHeight
barHeight = BarHeight 2

loop :: Vty -> Content -> State -> IO ()
loop vty content s' = ($ s') $ fix $ \go s ->
    if exit s
        then shutdown vty
        else do
            printState s
            e <- nextEvent vty
            case eventToAction e of
                Nothing -> go s
                Just a -> go $ changeState barHeight top a s
  where
    top = Line $ Map.size content - 1
    printState s = do
        pc <- getPage content $ viewPage barHeight s
        update vty $ picForImage $ renderPage pc <-> renderBar barHeight s

main :: IO ()
main = do
    vty <- mkVty defaultConfig
    let getBounds = displayBounds (outputIface vty)
    filename : _ <- getArgs
    withContent filename $ \content -> do
        (_, h) <- getBounds
        loop vty content $ boot $ Line h

-------------------------- Debug --------------------------

-- print a content
printContent :: Content -> IO ()
printContent = mapM_ print <=< sequence
