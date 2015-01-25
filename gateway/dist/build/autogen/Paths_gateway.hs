module Paths_gateway (
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
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/dodek/.cabal/bin"
libdir     = "/home/dodek/.cabal/lib/x86_64-linux-ghc-7.8.4/gateway-0.1.0.0"
datadir    = "/home/dodek/.cabal/share/x86_64-linux-ghc-7.8.4/gateway-0.1.0.0"
libexecdir = "/home/dodek/.cabal/libexec"
sysconfdir = "/home/dodek/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "gateway_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "gateway_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "gateway_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "gateway_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "gateway_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
