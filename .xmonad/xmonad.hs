import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Layout.PerWorkspace
import System.IO

-- Workspaces
myWorkspaces = [ "main", "web", "terminals", "mail", "im" ]

-- Workspace layouts
myLayouts = avoidStruts $
    onWorkspaces [ "web", "mail" ] Full $
    layoutHook defaultConfig

myManageHook = manageDocks <+> manageHook defaultConfig

myStartupHook :: X()
myStartupHook = do
    spawn "trayer-srg --edge top --align right --width 5 --height 18 --transparent true --alpha 0 --tint 0x101010 --SetDockType true --SetPartialStrut true"

main = do
    -- Start xmobar
    xmproc <- spawnPipe "xmobar"
    xmonad $ defaultConfig {
        workspaces = myWorkspaces,
        layoutHook = myLayouts,
        manageHook = myManageHook,
        -- Print status info and xmobar on state updates
        logHook = dynamicLogWithPP xmobarPP {
            ppOutput = hPutStrLn xmproc,
            ppTitle = xmobarColor "green" "" . shorten 50
        },
        startupHook = myStartupHook,
        -- Use super key instead of alt
        modMask = mod4Mask
    }
