{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Action
    ( Action (..)
    , eventToAction
    )
where

import Graphics.Vty
    ( Event (..)
    , Key (..)
    , Modifier (..)
    )

-- | Actions that can be taken by the user
data Action = Next | Prev | Resize Int Int | Quit

-- | Convert a Vty event to an action
eventToAction :: Event -> Maybe Action
eventToAction (EvKey KPageUp []) = Just Prev
eventToAction (EvKey KPageDown []) = Just Next
eventToAction (EvKey KDown []) = Just Next
eventToAction (EvKey KUp []) = Just Prev
eventToAction (EvKey (KChar 'q') []) = Just Quit
eventToAction (EvKey (KChar 'c') [MCtrl]) = Just Quit
eventToAction (EvResize w h) = Just $ Resize w h
eventToAction _ = Nothing
