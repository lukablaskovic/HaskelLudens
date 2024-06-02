{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_gloss (
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
version = Version [1,13,2,2] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "C:\\Users\\Luka\\Documents\\GitHub\\board-games-haskell\\.stack-work\\install\\851a09ad\\bin"
libdir     = "C:\\Users\\Luka\\Documents\\GitHub\\board-games-haskell\\.stack-work\\install\\851a09ad\\lib\\x86_64-windows-ghc-9.6.5\\gloss-1.13.2.2-16ZNb7pkabm3INkWzKjwmL"
dynlibdir  = "C:\\Users\\Luka\\Documents\\GitHub\\board-games-haskell\\.stack-work\\install\\851a09ad\\lib\\x86_64-windows-ghc-9.6.5"
datadir    = "C:\\Users\\Luka\\Documents\\GitHub\\board-games-haskell\\.stack-work\\install\\851a09ad\\share\\x86_64-windows-ghc-9.6.5\\gloss-1.13.2.2"
libexecdir = "C:\\Users\\Luka\\Documents\\GitHub\\board-games-haskell\\.stack-work\\install\\851a09ad\\libexec\\x86_64-windows-ghc-9.6.5\\gloss-1.13.2.2"
sysconfdir = "C:\\Users\\Luka\\Documents\\GitHub\\board-games-haskell\\.stack-work\\install\\851a09ad\\etc"

getBinDir     = catchIO (getEnv "gloss_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "gloss_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "gloss_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "gloss_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "gloss_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "gloss_sysconfdir") (\_ -> return sysconfdir)



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
