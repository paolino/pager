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

-- infinite effectful stream of `a` values
data Generator m a = Elem a (Generator m a) | Effect (m (Generator m a))

data Dir = Up | Down

next :: (Enum b) => Dir -> b -> b
next Up = pred
next Down = succ

generate
    :: (Monad m, Enum b)
    => (b -> m a)
    -- ^ Get a line by number
    -> b
    -- ^ Starting line number
    -> Dir
    -> Generator m (b, a)
generate pick from dir = go from
  where
    go i = Effect $ do
        line <- pick i
        pure $ Elem (i, line) $ go $ next dir i

-- expand each element of the generator into a non-empty list of elements
each
    :: forall m a c
     . (Monad m)
    => (a -> NonEmpty c)
    -> Generator m a
    -> Generator m c
each f (Elem x g) =
    let loop :: NonEmpty c -> Generator m c
        loop (y :| []) = Elem y $ each f g
        loop (y :| y' : ys) =
            let g'' = loop (y' :| ys)
             in Elem y g''
     in loop $ f x
each f (Effect m) = Effect $ each f <$> m

dropUntil :: (Monad m) => (a -> Bool) -> Generator m a -> Generator m a
dropUntil p e@(Elem x g) = if p x then e else dropUntil p g
dropUntil p (Effect m) = Effect $ dropUntil p <$> m

-- extract a finite number of values from the generator and the next one
collect
    :: forall m a
     . (Monad m)
    => Int
    -> Generator m a
    -> m ([a], a)
collect n g = go n g
  where
    go :: Int -> Generator m a -> m ([a], a)
    go m (Elem x g') =
        case m of
            0 -> pure ([], x)
            _ -> first (x :) <$> go (m - 1) g'
    go m (Effect e) = e >>= go m

--------------------------------------------------------------------------------
-- Cursor ----------------------------------------------------------------------
--------------------------------------------------------------------------------

type WLine = (Int, Int)

type Border = (WLine, WLine)

data Cursor a = Cursor
    { _cursor :: [a]
    , _cursorTop :: Border
    , _cursorBottom :: Border
    }
    deriving (Show)

scrollUp :: [a] -> Border -> Cursor a -> Cursor a
scrollUp ls x (Cursor _ t _) = Cursor ls x t

scrollDown :: [a] -> Border -> Cursor a -> Cursor a
scrollDown ls x (Cursor _ _ b) = Cursor ls b x

getLines
    :: (Monad m, Show a)
    => (Int -> m a)
    -- ^ Get a line by number
    -> (a -> NonEmpty a)
    -- ^ How to wrap lines
    -> WLine
    -- ^ Starting line number
    -> Dir
    -- ^ Direction to move
    -> Int
    -- ^ How many lines to collect
    -> m ([(WLine, a)], (WLine, a))
getLines pick wrap (l, l') dir count =
    collect count
        $ dropUntil (\(p, _) -> p `ord` (l, l'))
        $ each (\(i, a) -> verse $ first (i,) <$> NE.zip (0 :| [1 ..]) (wrap a))
        -- \$ showG
        $ generate pick l dir
  where
    ord = case dir of
        Up -> (<=)
        Down -> (>=)
    verse = case dir of
        Up -> NE.reverse
        Down -> id

scroll :: Dir -> ([(WLine, a)], (WLine, b)) -> Cursor a -> Cursor a
scroll dir (ls, (x, _)) =
    case dir of
        Up -> scrollUp als (x, l)
        Down -> scrollDown als (l, x)
  where
    (l, _) = last ls
    als = verse $ snd <$> ls
    verse = case dir of
        Up -> reverse
        Down -> id

mkCursor :: Border -> ([(WLine, a)], (WLine, a)) -> Cursor a
mkCursor b0 (ls, (x, _)) = Cursor (snd <$> ls) b0 (fst $ last ls, x)

--------------------------------------------------------------------------------
--- API ------------------------------------------------------------------------
--------------------------------------------------------------------------------

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
    -> (Int -> m a)
    -- ^ Get a line by number
    -> Int
    -- ^ How many rows to display at the beginning
    -> Int
    -- ^ How many columns to display at the beginning
    -> m (Page m a)
mkPage wrap pick rows cols = do
    f <- getLines pick (wrap cols) (0, 0) Down rows
    let c0 = mkCursor ((-1, 0), (0, 0)) f
    pure $ go c0 rows cols
  where
    update = getLines pick . wrap
    go :: Cursor a -> Int -> Int -> Page m a
    go cursor rows' cols' =
        Page
            { _page = _cursor cursor
            , _jump = \dir -> do
                f <- case dir of
                    Down ->
                        scroll Down
                            <$> update cols' (snd $ _cursorBottom cursor) Down rows'
                    Up ->
                        scroll Up
                            <$> update cols' (fst $ _cursorTop cursor) Up rows'
                pure $ go (f cursor) rows' cols'
            , _setCols = \n -> do
                as <- update n (snd $ _cursorTop cursor) Down rows'
                pure $ go (mkCursor (_cursorTop cursor) as) rows' n
            , _setRows = \n -> do
                as <- update cols' (snd $ _cursorTop cursor) Down n
                pure $ go (mkCursor (_cursorTop cursor) as) n cols'
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
