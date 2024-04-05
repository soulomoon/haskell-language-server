{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | Exposes the ghcide features as an HLS plugin
module Development.IDE.Plugin.HLS.GhcIde
  (
    descriptors
  , Log(..)
  ) where
import           Control.Monad.IO.Class
import           Development.IDE
import qualified Development.IDE.LSP.Notifications  as Notifications
import qualified Development.IDE.Plugin.Completions as Completions
import qualified Development.IDE.Plugin.TypeLenses  as TypeLenses
import           Ide.Types
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import           Text.Regex.TDFA.Text               ()

data Log
  = LogNotifications Notifications.Log
  | LogCompletions Completions.Log
  | LogTypeLenses TypeLenses.Log
  deriving Show

instance Pretty Log where
  pretty = \case
    LogNotifications msg -> pretty msg
    LogCompletions msg   -> pretty msg
    LogTypeLenses msg    -> pretty msg

descriptors :: Recorder (WithPriority Log) -> [PluginDescriptor IdeState]
descriptors recorder =
  [ descriptor "ghcide-hover-and-symbols",
    Completions.descriptor (cmapWithPrio LogCompletions recorder) "ghcide-completions",
    TypeLenses.descriptor (cmapWithPrio LogTypeLenses recorder) "ghcide-type-lenses",
    Notifications.descriptor (cmapWithPrio LogNotifications recorder) "ghcide-core"
  ]

-- ---------------------------------------------------------------------

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId desc)
  { pluginConfigDescriptor = defaultConfigDescriptor }
  where
    desc = "Provides core IDE features for Haskell"

-- ---------------------------------------------------------------------

