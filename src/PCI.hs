module PCI (PciDevice, pciId, className, vendorName, deviceName, getXorgPciString, domainId, slotId, functionId, getAllPciDevices) where

import Control.Monad.ST.Lazy (runST)
import Control.Monad.State
import Data.Char (isSpace)
import Data.List (elemIndex)
import System.Process

-- | a representation of a pci device
data PciDevice = PciDevice
  { -- | the pci id (like 0000:06:00.0)
    pciId :: PciID,
    -- | the class of the pci device
    className :: String,
    -- | the vendor of the pci device
    vendorName :: String,
    -- | the device name
    deviceName :: String
  }
  deriving (Show, Eq)

-- | a representation of a pci id (0000:06:00.0)
data PciID = PciID
  { -- | the domain id
    domainId :: Int,
    -- | the bus id
    busId :: Int,
    -- | the slot id
    slotId :: Int,
    -- | the function id
    functionId :: Int
  }
  deriving (Eq)

-- | formats 'PciID' as 0000:06:00.0
instance Show PciID where
  show id = (padInt 4 . domainId $ id) ++ ":" ++ (padInt 2 . busId $ id) ++ ":" ++ (padInt 2 . slotId $ id) ++ "." ++ (show . functionId $ id)

-- | Returns a 'PciID' in the format busId:slotId:functionId (like 06:00:0)
-- Used for the xorg config
getXorgPciString :: PciID -> String
getXorgPciString id = (padInt 2 . busId $ id) ++ ":" ++ (padInt 2 . slotId $ id) ++ ":" ++ (show . functionId $ id)

-- | returns all pci devices connected to this device
getAllPciDevices :: IO [PciDevice]
getAllPciDevices = parseLspci <$> runLspciCmd

-- | runs the lspci command and returns its output
runLspciCmd :: IO [String]
runLspciCmd = do
  output <- readCreateProcess (shell "lspci -vmm") ""
  return $ lines output

-- | parses the output from 'runLspciCmd' line by line. It wraps 'parseLspciState'
parseLspci :: [String] -> [PciDevice]
parseLspci = evalState parseLspciState

-- | This function does the actual parsing of the lines in a state monad
parseLspciState :: State [String] [PciDevice]
parseLspciState = do
  map <- covertToMap []
  let state = if length map > 0 then parseLspciState else return []
  devices <- state
  case parseMap map of
    Just pciDevice -> return $ pciDevice : devices
    Nothing -> return []

-- | creates a PciDevice from a map of key-value of pci values
-- The following keys are required: "Slot", "Class", "Vendor" and "Device"
parseMap :: [(String, String)] -> Maybe PciDevice
parseMap map = do
  slotName <- lookup "Slot" map
  pciId <- parseSlot slotName
  className <- lookup "Class" map
  vendorName <- lookup "Vendor" map
  deviceName <- lookup "Device" map
  return $ PciDevice pciId className vendorName deviceName

-- | parses a string as a 'PciID'. The expected string lookes like busId:slotId.functionId (06:00.0)
parseSlot :: String -> Maybe PciID
parseSlot slotName = do
  colonIndex <- elemIndex ':' slotName
  dotIndex <- elemIndex '.' slotName
  busId <- hexToInt $ take colonIndex slotName
  slotId <- hexToInt $ drop (colonIndex + 1) $ take dotIndex slotName
  functionId <- hexToInt $ drop (dotIndex + 1) slotName
  Just $ PciID 0 busId slotId functionId

-- | Takes one line, parses it as "key:value" and adds it to the given map. The map is then
-- returned by the state
covertToMap :: [(String, String)] -> State [String] [(String, String)]
covertToMap map = do
  lines <- get
  case lines of
    [] -> return map
    "" : ss -> put ss >> return map
    s : ss -> put ss >> covertToMap (splitLine s : map)
  where
    splitLine line = let (x, y) = splitOn ':' line in (trim x, trim y)

-- | Helper function
-- Splits the given string in a tuple when the given token is found
splitOn :: Eq a => a -> [a] -> ([a], [a])
splitOn token str = (x, drop 1 y)
  where
    (x, y) = span (/= token) str

-- | Trims whitspaces of a string at both ends
trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

-- | Converts a hex string into an int
hexToInt :: String -> Maybe Int
hexToInt [] = Just 0
hexToInt (x : xs) = do
  digit <- hexDigit x
  rest <- hexToInt xs
  Just $ digit * 16 ^ length xs + rest

-- | lookup map for 'hexToInt'
hexDigit :: Char -> Maybe Int
hexDigit '0' = Just 0
hexDigit '1' = Just 1
hexDigit '2' = Just 2
hexDigit '3' = Just 3
hexDigit '4' = Just 4
hexDigit '5' = Just 5
hexDigit '6' = Just 6
hexDigit '7' = Just 7
hexDigit '8' = Just 8
hexDigit '9' = Just 9
hexDigit 'a' = Just 10
hexDigit 'b' = Just 11
hexDigit 'c' = Just 12
hexDigit 'd' = Just 13
hexDigit 'e' = Just 14
hexDigit 'f' = Just 15
hexDigit _ = Nothing

-- | Padds the second number by the firsts number amount
padInt :: Int -> Int -> String
padInt pad i = padding ++ str
  where
    str = show i
    padding = take (pad - length str) $ repeat '0'
