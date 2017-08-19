{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_Apple (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Yotam\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\Yotam\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.0.2\\Apple-0.1.0.0-1vbohTjhdhE2er9MBBzxYh"
dynlibdir  = "C:\\Users\\Yotam\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.0.2"
datadir    = "C:\\Users\\Yotam\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.0.2\\Apple-0.1.0.0"
libexecdir = "C:\\Users\\Yotam\\AppData\\Roaming\\cabal\\Apple-0.1.0.0-1vbohTjhdhE2er9MBBzxYh"
sysconfdir = "C:\\Users\\Yotam\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Apple_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Apple_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Apple_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Apple_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Apple_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Apple_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)