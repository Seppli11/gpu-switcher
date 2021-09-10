module XOrgTemplate (Template, getEgpuTemplate, getDefaultEgpuXorgConf, enableTemplate, disableTemplate) where

import Control.Monad
import GHC.Conc.Sync (enableAllocationLimit)
import PCI
import System.Directory (createFileLink, doesFileExist, removeFile)

-- Represents a template
type Template = String

-- | creates the egpu template from the given pci device
getEgpuTemplate :: PciDevice -> Template
getEgpuTemplate device =
  "Section \"Module\"\n\
  \    Load \"modesetting\"\n\
  \EndSection\n\n\
  \Section \"Device\"\n\
  \    Identifier \"Device0\"\n\
  \    Driver \"amdgpu\"\n\
  \    BusID \"PCI:"
    ++ (getXorgPciString . pciId $ device)
    ++ "\"\n\
       \    Option \"AllowEmptyInitialConfiguration\"\n\
       \    Option \"AllowExternalGpus\" \"True\"\n\
       \EndSection"

-- | Returns the default location of the template file
getDefaultEgpuXorgConf :: String
getDefaultEgpuXorgConf = "/etc/X11/xorg.conf.d/99-gpu-switcher-egpu.conf"

-- | enables the given template by writing it to the given path
enableTemplate :: Template -> FilePath -> IO ()
enableTemplate template xorgTemplatePath = writeFile xorgTemplatePath template

-- | disables the template at the given filepath
disableTemplate :: FilePath -> IO ()
disableTemplate templatePath = doesFileExist templatePath >>= \exists -> when exists $ removeFile templatePath