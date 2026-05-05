-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}

module Development.IDE.Test
  ( Cursor
  , cursorPosition
  , requireDiagnostic
  , diagnostic
  , expectDiagnostics
  , expectDiagnosticsWithTags
  , ExpectedDiagnostic
  , ExpectedDiagnosticWithTag
  , expectNoMoreDiagnostics
  , expectMessages
  , expectCurrentDiagnostics
  , checkDiagnosticsForDoc
  , canonicalizeUri
  , standardizeQuotes
  , waitForAction
  , getInterfaceFilesDir
  , getFilesOfInterest
  , waitForBuildQueue
  , getStoredKeys
  , waitForCustomMessage
  , waitForGC
  , configureCheckProject
  , isReferenceReady
  , waitForTypecheck
  , referenceReady
  , waitForExpectedDiagnosticsFromDocs
  , waitForExpectedDiagnosticsFromDocsOne
  , filePathTextDocumentIdentifier
  , waitForExpectedDiagnosticsFromFilePath) where

import           Development.IDE.Test.Diagnostic
import           Test.Hls
