{-# LANGUAGE OverloadedStrings, FlexibleContexts, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module System.Posix.Directory.Stream (streamDirectory, find) where

-- Loosely based on
-- https://github.com/nh2/getDirectoryContents-benchmarks/blob/master/Main.hs

import Streaming (Stream, Of)
import qualified Streaming as S
import Streaming.Internal (Stream(..))
import qualified Streaming.Prelude as S
import System.IO.Error
import System.Posix.Files.ByteString
import System.Posix.ByteString.FilePath
import System.Posix.Directory.ByteString
import qualified Data.ByteString.Char8 as BS8
import Control.Exception (IOException)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource

-- | Return the directory contents as a 'Stream'. The stream does not
-- include @.@ and @..@ entries.
--
-- 'S.MonadResource' is used to close the POSIX directory
-- stream.
--
-- For an example on how to use this function, see the source code of the 'find'
-- function.
streamDirectory
  :: (MonadResource m, MonadCatch m)
  => RawFilePath
  -> Stream (Of RawFilePath) m ()
streamDirectory path = handle_errors $
  bracketStream
    (openDirStream path)
    closeDirStream
    loop
  where
  loop dirp = do
    e <- liftIO $ readDirStream dirp
    unless (BS8.null e) $ do
      unless (e == "." || e == "..") $ S.yield e
      loop dirp

  handle_errors :: (Functor f, MonadCatch m) => Stream f m a -> Stream f m a
  handle_errors = handle $ \e ->
    throwM
    . (`ioeSetFileName` (BS8.unpack path))
    . (`ioeSetLocation` "streamDirectory")
    $ e

-- | An example function that uses 'streamDirectory'. It traverses the
-- directory tree recursively, just like the @find@ command in UNIX.
--
-- It is somewhat simplistic in its handling of errors (they are ignored),
-- so you may want to write your own recursive traversal which does exactly
-- what you need.
find
  :: forall m . (MonadResource m, MonadCatch m)
  => RawFilePath
  -> Stream (Of RawFilePath) m ()
-- Note on error handling. The two most common errors one needs to deal
-- with are permission denied and dangling symlinks.
--
-- A dangling symlink error will be raised when calling getFileStatus. It
-- can be detected by calling getSymbolicLinkStatus.
--
-- A permission denied symlink will usually be raised when calling
-- streamDirectory.
find base = handle (\(_ :: IOException) -> return ()) $ loop base where

  loop :: RawFilePath -> Stream (Of RawFilePath) m ()
  loop path = do

    S.yield path

    is_dir <- liftIO $ isDirectory <$> getFileStatus path

    when is_dir $ do
      S.for (S.map ((path <> "/") <>) $ streamDirectory path) loop 

-- | This function was unfortunately removed from streaming starting with 0.2,
-- see <https://github.com/haskell-streaming/streaming/issues/36>.
bracketStream
  :: (Functor f, MonadResource m)
  => IO a -> (a -> IO ()) -> (a -> Stream f m b) -> Stream f m b
bracketStream alloc free inside = do
        (key, seed) <- S.lift (allocate alloc free)
        clean key (inside seed)
  where
    clean key = loop where
      loop str = case str of
        Return r -> Effect (release key >> return (Return r))
        Effect m -> Effect (liftM loop m)
        Step f   -> Step (fmap loop f)
{-#INLINABLE bracketStream #-}

-- Likewise, the following instances were removed from streaming

instance (MonadThrow m, Functor f) => MonadThrow (Stream f m) where
  throwM = S.lift . throwM
  {-#INLINE throwM #-}

instance (MonadCatch m, Functor f) => MonadCatch (Stream f m) where
  catch str f0 = go str
    where
    go p = case p of
      Step f      -> Step (fmap go f)
      Return  r   -> Return r
      Effect  m   -> Effect (catch (do
          p' <- m
          return (go p'))
       (\e -> return (f0 e)) )
  {-#INLINABLE catch #-}
