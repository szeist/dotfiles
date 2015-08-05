import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimpleFloat
import System.IO
import Control.Monad (when)
import Control.Applicative((<$>))
import System.Directory (doesFileExist)
import Graphics.X11.Xinerama (getScreenInfo)
import Graphics.ImageMagick.MagickWand
import Filesystem.Path.CurrentOS (decodeString)
import XMonad.Hooks.SetWMName

-- Workspaces
myWorkspaces :: [ String ]
myWorkspaces = [ "main", "web", "terminals", "mail", "im", "float" ]

-- Workspace layouts
myLayouts = avoidStruts $
    onWorkspaces [ "web", "mail" ] Full $
    onWorkspace  "float" simpleFloat $
    layoutHook defaultConfig

myManageHook :: ManageHook
myManageHook = composeAll [
        className =? "spotify" --> doFloat,
        className =? "Chromium-browser" --> doShift "web",
        className =? "Firefox" --> doShift "web",
        className =? "Thunderbird" --> doShift "mail",
        className =? "Skype" --> doShift "im"
    ]
    <+> manageDocks
    <+> manageHook defaultConfig

-- Startup programs
myStartupHook :: X()
myStartupHook = do
    spawn "trayer-srg --edge top --align right --width 5 --height 18 --transparent true --alpha 0 --tint 0x101010 --SetDockType true --SetPartialStrut true"
    spawn "xbindkeys"
    setWMName "LG3D"

-- Additional key mappings
myKeys :: [ (String, X ()) ]
myKeys = [ ("M-q", spawn "killall trayer-srg xbindkeys conky; xmonad --restart") ]

main :: IO ()
main = do
    -- Start xmobar
    xmproc <- spawnPipe "xmobar"
    -- Draw background image to all screen
    doesFileExist ".Xbackground.png" >>= flip when (drawBackground ".Xbackground.png")

    spawnConky 45 250

    xmonad $ defaultConfig {
        workspaces = myWorkspaces,
        layoutHook = myLayouts,
        manageHook = myManageHook,
        focusedBorderColor = "#00A300",
        -- Print status info and xmobar on state updates
        logHook = dynamicLogWithPP xmobarPP {
            ppOutput = hPutStrLn xmproc,
            ppTitle = xmobarColor "green" "" . shorten 50
        },
        startupHook = myStartupHook,
        -- Use super key instead of alt
        modMask = mod4Mask
    } `additionalKeysP` myKeys

-- Draw centered background image to all screens
drawBackground :: String -> IO ()
drawBackground backgroundImage = do
    (imageWidth, imageHeight) <- getImageDims backgroundImage
    screens <- openDisplay "" >>= getScreenInfo

    spawn $ "xloadimage -onroot -border black -fullscreen "
        ++ unwords (map (getOffsetArgs (imageWidth, imageHeight) backgroundImage) screens)

-- Get offset xloadimage args for screen image centering
getOffsetArgs :: (Int, Int) -> String -> Rectangle -> String
getOffsetArgs (imageWidth, imageHeight) backgroundImage screenInfo =
    "-at "
    ++ getOffsetStr (getCenterImageOffsets (imageWidth, imageHeight) screenInfo)
    ++ " "
    ++ backgroundImage

-- Format image offset string
getOffsetStr :: (Int, Int) -> String
getOffsetStr (offsetX, offsetY) = show offsetX ++ "," ++ show offsetY

-- Calculate screen center image offsets
getCenterImageOffsets :: (Int, Int) -> Rectangle -> (Int, Int)
getCenterImageOffsets (imageWidth, imageHeight) screenInfo =
    ( (fromIntegral (rect_width screenInfo) - imageWidth) `div` 2 + fromIntegral (rect_x screenInfo),
      (fromIntegral (rect_height screenInfo) - imageHeight) `div` 2 + fromIntegral (rect_y screenInfo) )

getImageDims :: String -> IO (Int, Int)
getImageDims fileName = withMagickWandGenesis $ do
    (_,wand) <- magickWand

    readImage wand $ decodeString fileName
    width <- getImageWidth wand
    height <- getImageHeight wand

    return (width, height)

spawnConky :: Int -> Int -> IO()
spawnConky topOffset rightOffset = getScreenDims 0 >>= \dims -> spawn $ "conky -x " ++ show (fst dims - rightOffset) ++ " -y " ++ show topOffset

getScreenDims :: Int -> IO (Int, Int)
getScreenDims screenIdx = get_dims <$> (!! screenIdx) <$> (openDisplay "" >>= getScreenInfo)
  where get_dims rect = (fromIntegral $ rect_width rect, fromIntegral $ rect_height rect)
