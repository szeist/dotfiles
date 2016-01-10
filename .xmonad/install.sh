#!/bin/bash

cabal install xmonad
cabal install xmobar --flags="with_xft with_utf8 with_iwlib with_alsa with_datezone with_xmp with_threaded"
