module Utils.Screen (getScreenDimensions, getScreens) where

import XMonad
import Graphics.X11.Xinerama (getScreenInfo)

getScreenDimensions :: Int -> IO (Int, Int)
getScreenDimensions screenIdx = get_dims <$> (!! screenIdx) <$> getScreens
  where get_dims rect = (fromIntegral $ rect_width rect, fromIntegral $ rect_height rect)

getScreens :: IO [ Rectangle ]
getScreens = openDisplay "" >>= getScreenInfo
