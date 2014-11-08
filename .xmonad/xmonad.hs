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

main = do
    -- Start xmobar
    xmproc <- spawnPipe "xmobar"
    xmonad $ defaultConfig {
        workspaces = myWorkspaces,
        layoutHook = myLayouts,
        -- Print status info and xmobar on state updates
        logHook = dynamicLogWithPP xmobarPP {
            ppOutput = hPutStrLn xmproc,
            ppTitle = xmobarColor "green" "" . shorten 50
        },
        -- Use super key instead of alt
        modMask = mod4Mask
    }
