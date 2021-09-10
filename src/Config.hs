{-# LANGUAGE DeriveGeneric #-}

module Config (Config, vendor, driver, getUserPath, getWorkingDirPath, getConfigPath, loadConfig, loadDefaultConfig) where

import Data.Aeson
import Data.Aeson.Types (FromJSON)
import GHC.Generics
import System.Directory

-- | defines the config json structure
data Config = Config
  { -- | the vendor name from lspci
    vendor :: String,
    -- | the driver name like amdgpu, intel or nvidia
    driver :: String
  }
  deriving (Generic, Show)

instance ToJSON Config where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Config

-- | returns the path to the user config
getUserPath :: String
getUserPath = "~/.config/gpu-switcher/config.json"

-- | the path of the config in the working dir (for debugging purposes)
getWorkingDirPath :: String
getWorkingDirPath = "./config/config.json"

-- | Tries to find a valid config. It first tries 'getUserPath' and then 'getWorkingDirPath'.
-- If both path aren't valid, Nothing is returned
getConfigPath :: IO (Maybe String)
getConfigPath = do
  userPathExists <- doesFileExist getUserPath
  workingDirExists <- doesFileExist getWorkingDirPath
  return $ f userPathExists workingDirExists
  where
    f userPathExists workingDirExists
      | userPathExists = Just getUserPath
      | workingDirExists = Just getWorkingDirPath
      | otherwise = Nothing

-- | loads the config at the given file path and returns either an error message or the config
loadConfig :: FilePath -> IO (Either String Config)
loadConfig = eitherDecodeFileStrict

-- | loads the config from the path returned by 'getConfigPath'. Like 'loadConfig' this function
-- returns either an error message or the config
loadDefaultConfig :: IO (Either String Config)
loadDefaultConfig = do
  path <- getConfigPath
  case path of
    Just path -> loadConfig path
    Nothing -> return $ Left $ "No config found at default positions (" ++ getUserPath ++ ")"
