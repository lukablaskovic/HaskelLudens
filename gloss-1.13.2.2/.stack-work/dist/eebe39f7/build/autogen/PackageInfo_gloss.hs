{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_gloss (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "gloss"
version :: Version
version = Version [1,13,2,2] []

synopsis :: String
synopsis = "Painless 2D vector graphics, animations and simulations."
copyright :: String
copyright = ""
homepage :: String
homepage = "http://gloss.ouroborus.net"
