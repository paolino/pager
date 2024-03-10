{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Action (Action (..), eventToAction)
import Control.Monad.Fix (fix)
import Control.Monad.State (MonadIO (..), evalStateT)
import File (Line (..), getLineN, withContent)
import Graphics.Vty
    ( Image
    , Output (displayBounds)
    , Vty (..)
    , black
    , blue
    , defAttr
    , defaultConfig
    , nextEvent
    , picForImage
    , resizeHeight
    , shutdown
    , string
    , update
    , vertCat
    , withBackColor
    , withForeColor
    , yellow
    , (<->)
    , (<|>)
    )
import Graphics.Vty.CrossPlatform (mkVty)
import Page
    ( Dir (..)
    , Page
    , dimenstionsS
    , getPageS
    , jumpS
    , mkPage
    , setColumnsS
    , setRowsS
    )
import System.Environment (getArgs)
import Wrap (wrap)

newtype BarHeight = BarHeight {getBarHeight :: Int}

loop :: Vty -> BarHeight -> Page IO String -> IO ()
loop vty (BarHeight barHeight) p = flip evalStateT p $ fix $ \go -> do
    text <- getPageS
    dims <- dimenstionsS
    liftIO
        $ update vty
        $ picForImage
        $ renderPage (fst dims - barHeight) text <-> renderBar dims
    e <- liftIO $ nextEvent vty
    case eventToAction e of
        Nothing -> go
        Just a -> do
            case a of
                Next -> jumpS Down >> go
                Prev -> jumpS Up >> go
                Resize w h -> do
                    setRowsS (h - barHeight)
                    setColumnsS w
                    go
                Quit -> liftIO $ shutdown vty

renderBar :: (Int, Int) -> Image
renderBar (h, w) = r <|> c
  where
    r = string (defAttr `withBackColor` blue) $ "rows " <> show h
    c =
        string (defAttr `withBackColor` yellow `withForeColor` black)
            $ "cols " <> show w

renderPage :: Int -> [String] -> Image
renderPage n = resizeHeight n . vertCat . fmap (string defAttr)

main :: IO ()
main = do
    filename : _ <- getArgs
    vty <- mkVty defaultConfig
    withContent filename $ \content -> do
        let pick = getLineN content . Line
        (w, h) <- displayBounds (outputIface vty)
        page <- mkPage (wrap (== ' ')) pick (h - 1) w
        loop vty (BarHeight 1) page
