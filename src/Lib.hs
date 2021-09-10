module Lib
  ( setupXorgConfig,
  )
where

import qualified Config
import Control.Exception.Base (SomeException (SomeException), throwIO)
import qualified PCI
import System.Directory (findExecutable)
import XOrgTemplate
  ( disableTemplate,
    enableTemplate,
    getDefaultEgpuXorgConf,
    getEgpuTemplate,
  )

-- | Finds the device with the given vendor name in the given list of PciDevices
-- If no device was found, then Nothing is returned, else the device
findDevice :: String -> [PCI.PciDevice] -> Maybe PCI.PciDevice
findDevice vendor devices = case filteredList of
  (x : _) -> Just x
  _ -> Nothing
  where
    filteredList = filter predicateFun devices
    predicateFun d = PCI.vendorName d == vendor

-- | Loads the config, checks if the egpu is connected, if connected, it will enable the template, else disable it
setupXorgConfig :: IO ()
setupXorgConfig = do
  config <- loadConfig
  devices <- PCI.getAllPciDevices
  let maybeDevice = findDevice (Config.vendor config) devices
  case maybeDevice of
    Just device -> do
      let egpuConf = getEgpuTemplate device
      enableTemplate egpuConf getDefaultEgpuXorgConf -- egpu found -> enable template
    Nothing -> disableTemplate getDefaultEgpuXorgConf -- no egpu found -> disabeling config

-- | loads the config in a io monad
loadConfig :: IO Config.Config
loadConfig = do
  eitherConfig <- Config.loadDefaultConfig
  case eitherConfig of
    Right config -> return config
    Left errorMsg -> error errorMsg