{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module File
    ( Line (..)
    , Content
    , getLineN
    , withContent
    )
where

import Data.Map (Map)
import qualified Data.Map as Map
import System.IO
    ( Handle
    , IOMode (ReadMode)
    , hGetLine
    , hGetPosn
    , hIsEOF
    , hSetPosn
    , withFile
    )

-- | Line number
newtype Line = Line Int
    deriving (Show, Eq, Ord, Num, Enum)

-- | Actions to get specific lines from a file
type Content = Map Line (IO String)

-- | Get the line at the given line number
getLineN :: Content -> Line -> IO String
getLineN m n = case Map.lookup n m of
    Nothing -> pure ""
    Just pos -> pos

-- return the action to read the line at the current handle position
getContentLine :: Handle -> IO (IO String)
getContentLine handle = do
    pos <- hGetPosn handle
    pure $ do
        hSetPosn pos
        l <- hGetLine handle
        pure $ filter (/= '\r') l

-- | Run an action against the content of a file
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
