-- |
--
-- This is a complete example of usage of 'Portager' library as a DSL for 
-- configuration of Gentoo portage
--
-- Niceness of the DSL depends on OverloadedStrings
--
{-# LANGUAGE OverloadedStrings #-}
module Main where

-- This is the only required import
import Portager

-- | Definition of a PackageSet named "base-core"
baseCore :: PortageR PackageSet
baseCore = "base-core" `with` do
    -- set defines a collection of packages
    pkgs [ "app-admin/sudo"
         , "app-editors/neovim" `with` do
               -- accept_keywords record with unstable for defined arch is created
               -- here "~amd64"
               unstable
               -- use flag "python" is enabled
               use [ "python" ]
               -- additionaly dependencies also require certain adjustments to 
               -- useflags and keywords
               dep [ "dev-python/neovim-python-client" `with` unstable
                   , "dev-libs/libtermkey" `with` unstable
                   , "dev-libs/libvterm" `with` unstable
                   , "dev-lua/lpeg" `with` do
                         unstable
                         use [ "luajit" ]
                   , "dev-libs/msgpack" `with` unstable
                   , "dev-lua/mpack" `with` do
                         unstable
                         use [ "luajit" ]
                   , "dev-libs/unibilium" `with` unstable
                   ]
         -- single line without 'do'
         , "sys-fs/cryptsetup" `with` use [ "urandom" ]
         -- multiple line without 'do'
         , "sys-apps/systemd" `with`
               dep [ "sys-apps/util-linux" `with` unstable ]
         ]
    -- set also defines a collection of dependencies
    -- dependencies are not in listed explicitly in a set file
    -- they are here for additional setup of useflags, keywords and licenses
    deps [ "dev-db/sqlite" `with` use [ "secure-delete" ]
         , "sys-libs/ncurses" `with` use [ "gpm" ]
         ]

baseTools :: PortageR PackageSet
baseTools = "base-tools" `with` do
    pkgs [ "app-admin/pwgen"
         -- multiple useflags, both enabled and disabled
         , "dev-vcs/git-annex" `with` use [ "-assistant", "-feed", "-quvi", "-s3", "-tahoe", "-torrentparser", "-webdav", "-xmpp", "-webapp" ]
         ]

games :: PortageR PackageSet
games = "games" `with` do
    pkgs [ "games-util/steam-meta" `with` do
               unstable
               dep [ "games-util/steam-client-meta" `with` unstable
                   , "games-util/steam-launcher" `with` unstable
                   , "games-util/steam-games-meta" `with` unstable
                   ]
         ]
    -- example of applying a useflag to multiple packages
    -- note that this list of dependencies is not complete :)
    deps $ map (`with` use [ "abi_x86_32" ])
         [ "dev-libs/openssl"
         , "media-libs/libjpeg-turbo"
         , "media-libs/libpng"
         , "media-libs/mesa"
         , "media-libs/tiff"
         , "virtual/glu"
         , "virtual/jpeg"
         , "virtual/libffi"
         , "virtual/libgudev"
         , "virtual/libiconv"
         , "virtual/libintl"
         , "virtual/libudev"
         , "virtual/libusb"
         , "virtual/opengl"
         , "virtual/pkgconfig"
         ]

-- configuration currently contains just architecture (either 'amd64' or 'x86')
cfg :: PortagerConfiguration
cfg = PortagerConfiguration { _arch = amd64 }

-- main should specify execution of 'portager' which encapsulates all the goodies
-- including command line parameters
main :: IO ()
main = portager cfg
    [ baseCore 
    , baseTools
    , games
    ]
