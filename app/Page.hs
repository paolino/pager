{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module Page where

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

-- infinite effectful stream of `a` values
data Generator m a = Elem a (Generator m a) | Effect (m (Generator m a))

data Dir = Up | Down

next :: (Enum b) => Dir -> b -> b
next Up = pred
next Down = succ

move :: (Num b) => Dir -> b -> b -> b
move Up x n = n - x
move Down x n = x + n

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

showG :: (Show a, Functor m) => Generator m a -> Generator m a
showG (Elem x g) = traceShow x $ Elem x $ showG g
showG (Effect m) = Effect $ showG <$> m

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

data Page m a = Page
    { _page :: [a]
    , _jump :: Dir -> m (Page m a)
    , _setCols :: Int -> m (Page m a)
    , _setRows :: Int -> m (Page m a)
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
        traceShow cursor
            $ Page
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
                }

--------------------------------------------------------------------------------
-- Monadic interface to the page -----------------------------------------------
--------------------------------------------------------------------------------

type n ~> m = forall a. n a -> m a

getPage :: (MonadState (Page n a) m) => m [a]
getPage = gets _page

modifyM :: (MonadState s m) => (s -> m s) -> m ()
modifyM f = get >>= f >>= put

setColumns :: (MonadState (Page n a) m) => (n ~> m) -> Int -> m ()
setColumns t n = modifyM $ \p -> t $ _setCols p n

setRows :: (MonadState (Page n a) m) => (n ~> m) -> Int -> m ()
setRows t n = modifyM $ \p -> t $ _setRows p n

jump :: (MonadState (Page n a) m) => (n ~> m) -> Dir -> m ()
jump t d = modifyM $ \p -> t $ _jump p d

withPage :: Page m a -> StateT (Page m a) m b -> m (b, Page m a)
withPage p f = runStateT f p

unfoldPage :: (Functor m) => Page m a -> StateT (Page m a) m b -> m b
unfoldPage f = fmap fst . withPage f

columnsL :: (Monad m) => Int -> StateT (Page m a) m ()
columnsL = setColumns lift

rowsL :: (Monad m) => Int -> StateT (Page m a) m ()
rowsL = setRows lift

jumpL :: (Monad m) => Dir -> StateT (Page m a) m ()
jumpL = jump lift

--------------------------------------------------------------------------------
-- Example usage ---------------------------------------------------------------
--------------------------------------------------------------------------------

testPage :: (Monad m) => m (Page m String)
testPage = mkPage splitter (\x -> pure $ concat $ replicate x (show x)) 3 3

splitter :: Int -> String -> NonEmpty String
splitter n x = case splitAt n x of
    (a, b) -> case b of
        [] -> a :| []
        _ -> a `NE.cons` splitter n b

unfoldTestPage :: StateT (Page IO String) IO [String] -> IO ()
unfoldTestPage f = do
    p <- testPage
    r <- unfoldPage p f
    putStrLn $ unlines r