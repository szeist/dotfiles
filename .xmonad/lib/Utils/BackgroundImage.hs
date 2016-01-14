module Utils.BackgroundImage (drawCenteredBackground) where

import XMonad
import Graphics.ImageMagick.MagickWand
import Filesystem.Path.CurrentOS (decodeString)
import Data.Text (pack)

import Utils.Screen

drawCenteredBackground :: String -> IO ()
drawCenteredBackground backgroundImage = do
    (imageWidth, imageHeight) <- getImageDimensions backgroundImage
    screens <- getScreens

    spawn $ "xloadimage -onroot -border black -fullscreen "
        ++ unwords (map (getOffsetArgs (imageWidth, imageHeight) backgroundImage) screens)


getOffsetArgs :: (Int, Int) -> String -> Rectangle -> String
getOffsetArgs (imageWidth, imageHeight) backgroundImage screenInfo =
    "-at "
    ++ getOffsetStr (getCenterImageOffsets (imageWidth, imageHeight) screenInfo)
    ++ " "
    ++ backgroundImage


getOffsetStr :: (Int, Int) -> String
getOffsetStr (offsetX, offsetY) = show offsetX ++ "," ++ show offsetY


getCenterImageOffsets :: (Int, Int) -> Rectangle -> (Int, Int)
getCenterImageOffsets (imageWidth, imageHeight) screenInfo =
    ( (fromIntegral (rect_width screenInfo) - imageWidth) `div` 2 + fromIntegral (rect_x screenInfo),
      (fromIntegral (rect_height screenInfo) - imageHeight) `div` 2 + fromIntegral (rect_y screenInfo) )


getImageDimensions :: String -> IO (Int, Int)
getImageDimensions fileName = withMagickWandGenesis $ do
    (_,wand) <- magickWand

    readImage wand $ pack fileName
    width <- getImageWidth wand
    height <- getImageHeight wand

    return (width, height)

