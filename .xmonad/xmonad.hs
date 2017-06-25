import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimpleFloat
import System.IO
import Control.Monad (when)
import System.Directory (doesFileExist)
import XMonad.Hooks.SetWMName
import XMonad.Config (def)
import XMonad.Layout.LayoutScreens

import Utils.BackgroundImage
import Utils.Screen
import Utils.Outputs


myDisplatConfigurations :: [[String]]
myDisplatConfigurations = [["DP2-2", "DP2-1", "eDP1"], ["VGA-1", "eDP-1"]]


myWorkspaces :: [ String ]
myWorkspaces = [ "main", "web", "work", "terminals", "im", "float" ]


myLayouts = avoidStruts $
    onWorkspaces [ "web", "work" ] Full $
    onWorkspace  "float" simpleFloat $
    layoutHook def


myManageHook :: ManageHook
myManageHook = composeAll [
        className =? "spotify" --> doFloat,
        className =? "google-chrome" --> doShift "web",
        className =? "Firefox" --> doShift "web",
        (className =? "Skype" <||> className =? "HipChat" <||> className =? "Slack") --> doShift "im"
    ]
    <+> manageDocks
    <+> manageHook def


myStartupHook :: X()
myStartupHook = do
    setWMName "LG3D"
    spawn "trayer --edge top --align right --width 5 --height 18 --transparent true --alpha 0 --tint 0x101010 --SetDockType true --SetPartialStrut true --monitor 1"
    spawn "xbindkeys"

myKeys :: [ (String, X ()) ]
myKeys =
  [ ("M-q", spawn "killall trayer xbindkeys conky; xmonad --restart"),
    ("M-S-l", spawn "dm-tool lock"),
    ("M-c", io $ applyConfiguration myDisplatConfigurations)
  ]


main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar"

    doesFileExist ".Xbackground.png" >>= flip when (drawCenteredBackground ".Xbackground.png")

    xmonad $ def {
        workspaces = myWorkspaces,
        layoutHook = myLayouts,
        manageHook = myManageHook,
        handleEventHook = handleEventHook defaultConfig <+> docksEventHook,
        focusedBorderColor = "#00A300",
        logHook = dynamicLogWithPP xmobarPP {
            ppOutput = hPutStrLn xmproc,
            ppTitle = xmobarColor "green" "" . shorten 50
        },
        startupHook = myStartupHook,
        modMask = mod4Mask
    } `additionalKeysP` myKeys
