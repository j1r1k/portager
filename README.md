# Portager - A Configuration DSL for Gentoo's Portage

Portager is here to help you maintain your Gentoo's Portage configuration sane. 

## Usage

See [[example/README.md]] for an example.

Portager library is not yet available on hackage. You can add it as a package dependency in `stack.yaml` (as in example project)

## Motivation and Examples

### Everyting in One File

If I want to install a package with changes to use flags and keywords, I have to add it to a set, amend package.use and amend package.accept_keywords. Three files to be changed just to install something non-standard (which is fairly standard on Gentoo).

### Changes of Useflags, Keywords for Dependencies

Taking Inkcape as an example of an atom, that requires some adjustments to use flags of packages it depends on.

Portager allows you to track those changes right where you define that you request Inkscape to be installed. If you ever decide to get rid of it, you might as well want to get rid of changes to its dependencies.

Having following complete Portager configuration:

```
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Portager

inkscape :: PortageR Package
inkscape = "media-gfx/inkscape" `with` do
               use [ "exif", "imagemagick" ]
               dep [ "app-text/poppler" `with` do
                       use [ "cairo" ]
                       dep [ "media-libs/libpng" `with` unstable ]
                   , "dev-python/pillow" `with` use [ "jpeg2k", "truetype" ]
                   ]

main :: IO ()
main = portager (PortagerConfiguration { _arch = amd64 })
    [ "graphics" `with` pkgs [ inkscape, "media-gfx/gimp" ] ]
```

And corresponding world_sets file:
```
@graphics
```

Results in creation of following files:

```
==> outputz/package.accept_keywords/graphics <==
media-libs/libpng ~amd64

==> outputz/package.use/01graphics <==
app-text/poppler cairo
dev-python/pillow jpeg2k truetype
media-gfx/inkscape exif imagemagick

==> outputz/sets/graphics <==
media-gfx/inkscape
media-gfx/gimp
```

Once we get rid of `inkscape`:

```
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Portager

main :: IO ()
main = portager (PortagerConfiguration { _arch = amd64 })
    [ "graphics" `with` pkgs [ "media-gfx/gimp" ] ]
```

It will result in creation of just a set file:

```
==> outputz/sets/graphics <==
media-gfx/gimp
```

### Clear Separation of Configuration of Different Sets

Portager operates on level of sets. Set is a collection of atoms that can be requested to be installed by `emerge`. This can be made permanent by adding the set to a world sets file (`/var/lib/portage/world_sets`).

This has a benefit that when you later on decide you don't need a set of packages anymore, you just get rid of it, with all useflag, keyword and license settings.

Similarly to global useflags defined by adding them to `USE` variable, you can define set global useflags in Portager (this results in appending the useflag to all atoms listed in a set, even those for which it is not valid).

Having following complete Portager configuration:

```

```
And corresponding world_sets file:

```
desktop-media
```

Results in creation of following files:

```
==> outputz/package.use/01desktop-media <==
media-gfx/gpicview X
media-video/vlc X

==> outputz/sets/desktop-media <==
media-gfx/gpicview
media-video/vlc
```

Atom `media-gfx/gpicview` has `X` useflag enabled even though it is not defined for the it.

## Functionality

Import `Portager` and run `portager` in your `main` function. It accepts two parameters:
- a configuration (currently contains only selected architecture - e.g. `amd64`)
- a list of `PortageR PackageSet`s. The type is as complex as it is in order to push down the sequencing down to the library function.

### Order of Sets Matters

The order of sets is important if you define overriding useflag settings.

Having following complete Portager configuration:

```
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Portager

main :: IO ()
main = portager (PortagerConfiguration { _arch = amd64 })
    [ "console-tools" `with` pkgs ["app-editors/vim" `with` use ["-X"]]
    , "desktop-tools" `with` pkgs ["app-editors/vim" `with` use ["X"]]
    ]
```

And corresponding world_sets file (here order does not matter for Portager):

```
@console-tools
@desktop-tools
```

Results in creation of following files:

```
==> package.use/01console-tools <==
app-editors/vim -X

==> package.use/02desktop-tools <==
app-editors/vim X

==> sets/console-tools <==
app-editors/vim

==> sets/desktop-tools <==
app-editors/vim
```

If you ask emerge to install `app-editors/vim` it will be installed with `X` useflag enabled.
