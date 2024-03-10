{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module Page
    ( Page (..)
    , Dir (..)
    , mkPage

      -- * StateT monadic interface to the page
    , getPageS
    , setColumnsS
    , setRowsS
    , jumpS
    , dimenstionsS
    )
where

import Control.Monad.State
    ( MonadState (..)
    , MonadTrans (..)
    , StateT (..)
    , gets
    )
import Data.Bifunctor (Bifunctor (..), first)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Debug.Trace (traceShow)
import Generator (Dir (..), collect, dropUntil, each, generate)

--------------------------------------------------------------------------------
-- Cursor ----------------------------------------------------------------------
--------------------------------------------------------------------------------

data Border b = End b | Border b b
    deriving (Show)

swapBorder :: Border a -> Border a
swapBorder (End x) = End x
swapBorder (Border x y) = Border y x

data Cursor a b = Cursor
    { _cursor :: [a]
    , _cursorTop :: Border b
    , _cursorBottom :: Border b
    }
    deriving (Show)

scrollUp :: [a] -> Border b -> Cursor a b -> Cursor a b
scrollUp ls x (Cursor _ t _) = Cursor ls x $ swapBorder t

scrollDown :: [a] -> Border b -> Cursor a b -> Cursor a b
scrollDown ls x (Cursor _ _ b) = Cursor ls (swapBorder b) x

data PageData a b = PageData
    { _wraps :: [a]
    , _lastIndex :: b
    , _nextIndex :: Maybe b
    }

scroll :: Dir -> PageData a b -> Cursor a b -> Cursor a b
scroll dir (PageData xs l mn) = scrolling xs'
    $ case mn of
        Just x -> Border l x
        Nothing -> End l
  where
    scrolling = case dir of
        Up -> scrollUp
        Down -> scrollDown
    xs' = verse xs
    verse = case dir of
        Up -> reverse
        Down -> id

startCursor :: Border b -> PageData a b -> Cursor a b
startCursor b0 (PageData xs l mn) = Cursor xs b0
    $ case mn of
        Just x -> Border l x
        Nothing -> End l

endCursor :: Border b -> PageData a b -> Cursor a b
endCursor b0 (PageData xs l mn) = flip (Cursor (reverse xs)) b0
    $ case mn of
        Just x -> Border l x
        Nothing -> End l

fromTop :: Cursor a b -> b
fromTop (Cursor _ (End x) _) = x
fromTop (Cursor _ (Border x _) _) = x

--------------------------------------------------------------------------------
--- API ------------------------------------------------------------------------
--------------------------------------------------------------------------------

type Wrap = (Int, Int)

getLines
    :: (Monad m, Show a)
    => (Int -> m (Maybe a))
    -- ^ Get a line by number
    -> (a -> NonEmpty a)
    -- ^ How to wrap lines
    -> Wrap
    -- ^ Starting line number
    -> Dir
    -- ^ Direction to move
    -> Int
    -- ^ How many lines to collect
    -> m (PageData a Wrap)
getLines pick wrap (l, l') dir count =
    fmap mkPageData
        $ collect count
        $ dropUntil (\(p, _) -> p `ord` (l, l'))
        $ each (\(i, a) -> verse $ first (i,) <$> NE.zip (0 :| [1 ..]) (wrap a))
        $ generate pick l dir
  where
    mkPageData (xs, mn) = PageData (snd <$> xs) l $ fst <$> mn
      where
        l = fst $ last xs
    ord = case dir of
        Up -> (<=)
        Down -> (>=)
    verse = case dir of
        Up -> NE.reverse
        Down -> id

data Page m a = Page
    { _page :: [a]
    , _jump :: Dir -> m (Page m a)
    , _setCols :: Int -> m (Page m a)
    , _setRows :: Int -> m (Page m a)
    , _dimenstions :: (Int, Int)
    }

mkPage
    :: forall m a
     . (Monad m, Show a)
    => (Int -> a -> NonEmpty a)
    -- ^ How to wrap lines
    -> (Int -> m (Maybe a))
    -- ^ Get a line by number
    -> Int
    -- ^ How many rows to display at the beginning
    -> Int
    -- ^ How many columns to display at the beginning
    -> m (Page m a)
mkPage wrap pick rows cols = do
    f <- update cols (0, 0) Down rows
    pure $ go (startCursor (End (0, 0)) f) rows cols
  where
    update :: Int -> Wrap -> Dir -> Int -> m (PageData a Wrap)
    update = getLines pick . wrap
    refresh rows' cols' cursor = do
        as <- update cols' (fromTop cursor) Down rows'
        pure $ go (startCursor (_cursorTop cursor) as) rows' cols'

    go :: Cursor a Wrap -> Int -> Int -> Page m a
    go cursor rows' cols' =
        Page
            { _page = _cursor cursor
            , _jump = \dir -> do
                f <- case dir of
                    Down ->
                        case _cursorBottom cursor of
                            End p -> do
                                as <- update cols' p Up (rows' - 1)
                                pure $ const $ endCursor (End p) as
                            Border _ n ->
                                scroll Down
                                    <$> update cols' n Down rows'
                    Up ->
                        case _cursorTop cursor of
                            End p -> do
                                as <- update cols' p Down rows'
                                pure $ const $ startCursor (End p) as
                            Border _ n ->
                                scroll Up
                                    <$> update cols' n Up rows'
                pure $ go (f cursor) rows' cols'
            , _setCols = \n -> refresh rows' n cursor
            , _setRows = \n -> refresh n cols' cursor
            , _dimenstions = (rows', cols')
            }

--------------------------------------------------------------------------------
-- Monadic interface to the page -----------------------------------------------
--------------------------------------------------------------------------------

type n ~> m = forall a. n a -> m a

getPageS :: (MonadState (Page n a) m) => m [a]
getPageS = gets _page

modifyM :: (MonadState s m) => (s -> m s) -> m ()
modifyM f = get >>= f >>= put

setColumns :: (MonadState (Page n a) m) => (n ~> m) -> Int -> m ()
setColumns t n = modifyM $ \p -> t $ _setCols p n

setRows :: (MonadState (Page n a) m) => (n ~> m) -> Int -> m ()
setRows t n = modifyM $ \p -> t $ _setRows p n

jump :: (MonadState (Page n a) m) => (n ~> m) -> Dir -> m ()
jump t d = modifyM $ \p -> t $ _jump p d

setColumnsS :: (Monad m) => Int -> StateT (Page m a) m ()
setColumnsS = setColumns lift

setRowsS :: (Monad m) => Int -> StateT (Page m a) m ()
setRowsS = setRows lift

jumpS :: (Monad m) => Dir -> StateT (Page m a) m ()
jumpS = jump lift

dimenstionsS :: (Monad m) => StateT (Page m a) m (Int, Int)
dimenstionsS = gets _dimenstions
