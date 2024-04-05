{-# LANGUAGE OverloadedStrings #-}


module Ide.Plugin.Core(descriptor, CoreLog) where

import           Control.Monad.IO.Class        (liftIO)
import           Data.Maybe                    (fromMaybe)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Development.IDE
import           Development.IDE.Core.Actions  (refsAtPoint, workspaceSymbols)
import qualified Development.IDE.Core.Shake    as Shake
import           Ide.Plugin.Error              (getNormalizedFilePathE)
import           Ide.Types
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types   (ReferenceParams (..),
                                                TextDocumentIdentifier (..),
                                                WorkspaceSymbolParams (..),
                                                type (|?) (InL))

data CoreLog
  = LogShake Shake.Log
  | CoreLogMsg Text

instance Pretty CoreLog where
  pretty theLog = case theLog of
    LogShake shakeLog -> pretty shakeLog
    CoreLogMsg msg    -> "Core Message: " <> pretty msg



descriptor :: Recorder (WithPriority CoreLog) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId =
  (defaultPluginDescriptor plId "Provides core IDE features for Haskell")
    {
        Ide.Types.pluginHandlers =
            mkPluginHandler SMethod_WorkspaceSymbol (wsSymbols recorder)
            <> mkPluginHandler SMethod_TextDocumentReferences references
    }


wsSymbols :: Recorder (WithPriority CoreLog) -> PluginMethodHandler IdeState Method_WorkspaceSymbol
wsSymbols logger ide _ (WorkspaceSymbolParams _ _ query) = liftIO $ do
  logWith logger Debug $ CoreLogMsg $ "Workspace symbols request: " <> query
  runIdeAction "WorkspaceSymbols" (shakeExtras ide) $ InL . fromMaybe [] <$> workspaceSymbols query



references :: PluginMethodHandler IdeState Method_TextDocumentReferences
references ide _ (ReferenceParams (TextDocumentIdentifier uri) pos _ _ _) = do
  nfp <- getNormalizedFilePathE uri
  liftIO $ logDebug (ideLogger ide) $
        "References request at position " <> T.pack (showPosition pos) <>
        " in file: " <> T.pack (show nfp)
  InL <$> (liftIO $ runAction "references" ide $ refsAtPoint nfp pos)
