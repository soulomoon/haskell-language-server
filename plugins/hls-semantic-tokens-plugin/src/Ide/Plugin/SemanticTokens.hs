{-# LANGUAGE OverloadedStrings #-}


module Ide.Plugin.SemanticTokens (descriptor) where

import           Development.IDE
import qualified Ide.Plugin.SemanticTokens.Internal as Internal
import           Ide.Plugin.SemanticTokens.Types
import           Ide.Types
import           Language.LSP.Protocol.Message

-- I hope that does mean much more sense now, only fire at the point would give a bit more than it should
descriptor :: Recorder (WithPriority SemanticLog) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId =
  (defaultPluginDescriptor plId "Provides semantic tokens")
    { Ide.Types.pluginHandlers =
        mkPluginHandler SMethod_TextDocumentSemanticTokensFull (Internal.semanticTokensFull recorder)
        <> mkPluginHandler SMethod_TextDocumentSemanticTokensFullDelta (Internal.semanticTokensFullDelta recorder),
      Ide.Types.pluginRules = Internal.getSemanticTokensRule recorder,
      pluginConfigDescriptor =
        defaultConfigDescriptor
          { configInitialGenericConfig = (configInitialGenericConfig defaultConfigDescriptor) {plcGlobalOn = False}
          , configCustomConfig = mkCustomConfig Internal.semanticConfigProperties
          }
    }
