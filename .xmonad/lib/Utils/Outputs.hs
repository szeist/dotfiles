module Utils.Outputs (applyConfiguration) where

import Data.Maybe (fromJust)
import Data.List (maximumBy, find)
import Data.Function (on)
import Graphics.X11.Xrandr
import XMonad


data OutputInfo = OutputInfo {
  output :: XRROutputInfo,
  mode :: XRRModeInfo
}


applyConfiguration :: [[String]] -> IO()
applyConfiguration configurations = do
  dpy <- openDisplay ""
  connectedOutputs <- getOutputInfos dpy

  let configuration = find (matchConfiguration $ map (\o -> xrr_oi_name(output o)) connectedOutputs) configurations

  removeDisconnected dpy

  case configuration of
    Nothing -> mapM_ (spawn . getConnectedCommand) connectedOutputs
    Just conf -> mapM_ spawn $ getDisplayConfigurationCommands $ findOutputConfigurations conf connectedOutputs


matchConfiguration :: [String] -> [String] -> Bool
matchConfiguration connectedNames = all (`elem` connectedNames)


removeDisconnected dpy = do
  disconnectedOutputs <- getDisconnectedOutputs dpy
  mapM_ (spawn . getDisconnectedCommand) disconnectedOutputs


getDisconnectedCommand :: XRROutputInfo -> [Char]
getDisconnectedCommand outputInfo =
  "xrandr --output " ++
  xrr_oi_name outputInfo ++
  " --off"


findOutputConfigurations :: [String] -> [OutputInfo] -> [OutputInfo]
findOutputConfigurations conf outputs = map (\n -> fromJust $ find (\o -> xrr_oi_name(output o) == n) outputs) conf


getDisplayConfigurationCommands :: [OutputInfo] -> [String]
getDisplayConfigurationCommands outputs = getFirstOutputCommand outputs : getAdditionalOutputCommands outputs


getFirstOutputCommand :: [OutputInfo] -> String
getFirstOutputCommand outputs = getConnectedCommand $ last outputs


getAdditionalOutputCommands :: [OutputInfo] -> [String]
getAdditionalOutputCommands outputs = map getAlignedOutputCommand $ reverse $ zip outputs $ tail outputs


getConnectedCommand :: OutputInfo -> [Char]
getConnectedCommand outputInfo =
  "xrandr --output " ++
  xrr_oi_name(output outputInfo) ++
  " --mode " ++
  xrr_mi_name(mode outputInfo)


getAlignedOutputCommand :: (OutputInfo, OutputInfo) -> String
getAlignedOutputCommand (cur, nxt) = getConnectedCommand cur ++ " --left-of " ++ xrr_oi_name(output nxt)


getOutputInfos :: Display -> IO [OutputInfo]
getOutputInfos dpy = do
  outputs <- getConnectedOutputs dpy
  mapM (getOutputInfo dpy) outputs


getConnectedOutputs :: Display -> IO [XRROutputInfo]
getConnectedOutputs dpy = filter (\x -> xrr_oi_connection x == 0) <$> getOutputs dpy


getDisconnectedOutputs :: Display -> IO [XRROutputInfo]
getDisconnectedOutputs dpy = filter (\x -> xrr_oi_connection x /= 0) <$> getOutputs dpy


getOutputInfo :: Display -> XRROutputInfo -> IO OutputInfo
getOutputInfo dpy outputInfo = do
  maxResolutionMode <- getHighestResolution dpy $ xrr_oi_modes outputInfo
  return OutputInfo { output = outputInfo, mode = maxResolutionMode }


getHighestResolution :: Display -> [RRMode] -> IO XRRModeInfo
getHighestResolution dpy modeIds = do
  resources <- getResources dpy
  return $
    maximumBy (compare `on` (\modeInfo -> xrr_mi_width modeInfo * xrr_mi_height modeInfo)) $
      filter (\modeInfo -> xrr_mi_id modeInfo `elem` modeIds) $
        xrr_sr_modes resources


getOutputs :: Display -> IO [XRROutputInfo]
getOutputs dpy = do
  resources <- getResources dpy
  mapM (\x -> fromJust <$> xrrGetOutputInfo dpy resources x) $ xrr_sr_outputs resources


getResources :: Display -> IO XRRScreenResources
getResources dpy = do
  rootw <- rootWindow dpy $ defaultScreen dpy
  fromJust <$> xrrGetScreenResources dpy rootw
