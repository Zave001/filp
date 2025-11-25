{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_filp (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "D:\\projects\\filp\\.stack-work\\install\\5e36bf14\\bin"
libdir     = "D:\\projects\\filp\\.stack-work\\install\\5e36bf14\\lib\\x86_64-windows-ghc-9.4.8\\filp-0.1.0.0-6J7HtZkX3Nt71gqYHIf1pR-filp"
dynlibdir  = "D:\\projects\\filp\\.stack-work\\install\\5e36bf14\\lib\\x86_64-windows-ghc-9.4.8"
datadir    = "D:\\projects\\filp\\.stack-work\\install\\5e36bf14\\share\\x86_64-windows-ghc-9.4.8\\filp-0.1.0.0"
libexecdir = "D:\\projects\\filp\\.stack-work\\install\\5e36bf14\\libexec\\x86_64-windows-ghc-9.4.8\\filp-0.1.0.0"
sysconfdir = "D:\\projects\\filp\\.stack-work\\install\\5e36bf14\\etc"

getBinDir     = catchIO (getEnv "filp_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "filp_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "filp_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "filp_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "filp_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "filp_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
