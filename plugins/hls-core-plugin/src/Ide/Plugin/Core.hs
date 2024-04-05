{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Ide.Plugin.Core(descriptor, CoreLog) where

import           Control.Monad.IO.Class          (liftIO)
import           Data.Maybe                      (fromMaybe)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Development.IDE
import qualified Development.IDE.Core.Shake      as Shake
import           Ide.Plugin.Core.Actions         (refsAtPoint, workspaceSymbols)
import           Ide.Plugin.Core.HoverDefinition
import           Ide.Plugin.Core.Outline         (moduleOutline)
import           Ide.Plugin.Error                (getNormalizedFilePathE)
import           Ide.Types
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types     (DefinitionParams (..),
                                                  DocumentHighlightParams (..),
                                                  HoverParams (..),
                                                  ReferenceParams (..),
                                                  TextDocumentIdentifier (..),
                                                  TextDocumentPositionParams (..),
                                                  TypeDefinitionParams (..),
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
                  mkPluginHandler SMethod_TextDocumentDocumentSymbol moduleOutline
                  <> mkPluginHandler SMethod_TextDocumentDefinition (\ide _ DefinitionParams{..} ->
                      gotoDefinition ide TextDocumentPositionParams{..})
                  <> mkPluginHandler SMethod_TextDocumentTypeDefinition (\ide _ TypeDefinitionParams{..} ->
                      gotoTypeDefinition ide TextDocumentPositionParams{..})
                  <> mkPluginHandler SMethod_TextDocumentDocumentHighlight (\ide _ DocumentHighlightParams{..} ->
                      documentHighlight ide TextDocumentPositionParams{..})
                  <> mkPluginHandler SMethod_TextDocumentHover hover'
                  <> mkPluginHandler SMethod_WorkspaceSymbol (wsSymbols recorder)
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


hover' :: PluginMethodHandler IdeState Method_TextDocumentHover
hover' ideState _ HoverParams{..} = do
    liftIO $ logDebug (ideLogger ideState) "GhcIde.hover entered (ideLogger)" -- AZ
    hover ideState TextDocumentPositionParams{..}
