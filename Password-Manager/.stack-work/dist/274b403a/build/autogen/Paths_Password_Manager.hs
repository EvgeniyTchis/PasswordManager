{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Password_Manager (
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

bindir     = "C:\\Users\\Jason\\desktop\\PasswordManager\\Password-Manager\\.stack-work\\install\\dc6fdff9\\bin"
libdir     = "C:\\Users\\Jason\\desktop\\PasswordManager\\Password-Manager\\.stack-work\\install\\dc6fdff9\\lib\\x86_64-windows-ghc-8.10.4\\Password-Manager-0.1.0.0-7tFv0oS3Zmq9mwF8CtcIhb"
dynlibdir  = "C:\\Users\\Jason\\desktop\\PasswordManager\\Password-Manager\\.stack-work\\install\\dc6fdff9\\lib\\x86_64-windows-ghc-8.10.4"
datadir    = "C:\\Users\\Jason\\desktop\\PasswordManager\\Password-Manager\\.stack-work\\install\\dc6fdff9\\share\\x86_64-windows-ghc-8.10.4\\Password-Manager-0.1.0.0"
libexecdir = "C:\\Users\\Jason\\desktop\\PasswordManager\\Password-Manager\\.stack-work\\install\\dc6fdff9\\libexec\\x86_64-windows-ghc-8.10.4\\Password-Manager-0.1.0.0"
sysconfdir = "C:\\Users\\Jason\\desktop\\PasswordManager\\Password-Manager\\.stack-work\\install\\dc6fdff9\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Password_Manager_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Password_Manager_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Password_Manager_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Password_Manager_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Password_Manager_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Password_Manager_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
