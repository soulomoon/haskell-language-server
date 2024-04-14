{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Display information on hover.
module Ide.Plugin.Core.HoverDefinition
    (
    -- * For haskell-language-server
    hover
    , gotoDefinition
    , gotoTypeDefinition
    , documentHighlight
    -- , references
    -- , wsSymbols
    ) where

import           Control.Monad.Except           (ExceptT)
import           Control.Monad.IO.Class
import           Data.Maybe                     (fromMaybe)
import           Development.IDE.Core.Actions
import           Development.IDE.Core.Rules
import           Development.IDE.Core.Shake
import           Development.IDE.Types.Location
import           Ide.Logger
import           Ide.Plugin.Error
import           Ide.Types
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import qualified Language.LSP.Server            as LSP

import qualified Data.Text                      as T

gotoDefinition :: IdeState -> TextDocumentPositionParams -> ExceptT PluginError (LSP.LspM c) (MessageResult Method_TextDocumentDefinition)
hover          :: IdeState -> TextDocumentPositionParams -> ExceptT PluginError (LSP.LspM c) (Hover |? Null)
gotoTypeDefinition :: IdeState -> TextDocumentPositionParams -> ExceptT PluginError (LSP.LspM c) (MessageResult Method_TextDocumentTypeDefinition)
documentHighlight :: IdeState -> TextDocumentPositionParams -> ExceptT PluginError (LSP.LspM c) ([DocumentHighlight] |? Null)
gotoDefinition = request "Definition" getDefinition (InR $ InR Null) (InL . Definition. InR)
gotoTypeDefinition = request "TypeDefinition" getTypeDefinition (InR $ InR Null) (InL . Definition. InR)
hover          = request "Hover"      getAtPoint     (InR Null)     foundHover
documentHighlight = request "DocumentHighlight" highlightAtPoint (InR Null) InL


foundHover :: (Maybe Range, [T.Text]) -> Hover |? Null
foundHover (mbRange, contents) =
  InL $ Hover (InL $ MarkupContent MarkupKind_Markdown $ T.intercalate sectionSeparator contents) mbRange

-- | Respond to and log a hover or go-to-definition request
request
  :: T.Text
  -> (NormalizedFilePath -> Position -> IdeAction (Maybe a))
  -> b
  -> (a -> b)
  -> IdeState
  -> TextDocumentPositionParams
  -> ExceptT PluginError (LSP.LspM c) b
request label getResults notFound found ide (TextDocumentPositionParams (TextDocumentIdentifier uri) pos) = liftIO $ do
    mbResult <- case uriToFilePath' uri of
        Just path -> logAndRunRequest label getResults ide pos path
        Nothing   -> pure Nothing
    pure $ maybe notFound found mbResult

logAndRunRequest :: T.Text -> (NormalizedFilePath -> Position -> IdeAction b) -> IdeState -> Position -> String -> IO b
logAndRunRequest label getResults ide pos path = do
  let filePath = toNormalizedFilePath' path
  logDebug (ideLogger ide) $
    label <> " request at position " <> T.pack (showPosition pos) <>
    " in file: " <> T.pack path
  runIdeAction (T.unpack label) (shakeExtras ide) (getResults filePath pos)
