#!/bin/bash

# Dependencies
cabal install system-filepath
cabal install imagemagick

cabal install xmonad
cabal install xmonad-contrib
cabal install xmobar --flags="with_xft with_utf8 with_iwlib with_alsa with_datezone with_xmp with_threaded"
