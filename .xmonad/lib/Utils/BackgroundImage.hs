module Utils.BackgroundImage (drawCenteredBackground) where

import XMonad
import Filesystem.Path.CurrentOS (decodeString)
import Data.Text (pack)
import Data.Conduit (($$))
import Data.Conduit.Binary as CB
import Data.Conduit.ImageSize as CI
import Control.Monad.Trans.Resource (runResourceT)

import Utils.Screen

-- FIXME don't use xloadimage
-- FIXME (handle errors)
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


-- FIXME (handle errors)
getImageDimensions :: String -> IO (Int, Int)
getImageDimensions imagePath = runResourceT $ do
    msize <- CB.sourceFile imagePath $$ CI.sinkImageSize
    size <- msize
    return (width size, height size)
