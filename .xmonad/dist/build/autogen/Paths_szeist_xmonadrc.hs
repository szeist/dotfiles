module Paths_szeist_xmonadrc (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/iszenasi/.cabal/bin"
libdir     = "/home/iszenasi/.cabal/lib/x86_64-linux-ghc-7.10.3/szeist-xmonadrc-0.1.0.0-0Ay9AQNHj49E0X62SRWB21"
datadir    = "/home/iszenasi/.cabal/share/x86_64-linux-ghc-7.10.3/szeist-xmonadrc-0.1.0.0"
libexecdir = "/home/iszenasi/.cabal/libexec"
sysconfdir = "/home/iszenasi/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "szeist_xmonadrc_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "szeist_xmonadrc_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "szeist_xmonadrc_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "szeist_xmonadrc_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "szeist_xmonadrc_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
