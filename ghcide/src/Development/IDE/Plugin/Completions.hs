{-# LANGUAGE CPP          #-}
{-# LANGUAGE TypeFamilies #-}
#include "ghc-api-version.h"

module Development.IDE.Plugin.Completions
    (
      localCompletionsDescriptor,
      nonLocalCompletionsDescriptor,
      keywordCompletionsDescriptor
    ,pragmaCompletionsDescriptor) where
import Language.Haskell.LSP.Types
import qualified Language.Haskell.LSP.Core as LSP
import qualified Language.Haskell.LSP.VFS as VFS

import Development.Shake.Classes
import Development.Shake
import GHC.Generics
import Development.IDE.Core.Service
import Development.IDE.Core.PositionMapping
import Development.IDE.Plugin.Completions.Logic
import Development.IDE.Types.Location
import Development.IDE.Core.RuleTypes
import Development.IDE.Core.Shake
import Development.IDE.GHC.Compat

import Development.IDE.GHC.Util
import TcRnDriver (tcRnImportDecls)
import Data.Maybe
import Ide.Plugin.Config (Config (completionSnippetsOn))
import Ide.PluginUtils (getClientConfig)
import Ide.Types (PluginDescriptor (pluginCompletionProvider, pluginRules), PluginId, defaultPluginDescriptor)
import Development.IDE.Spans.LocalBindings
import Control.Monad
import Development.IDE.Types.Options
import Language.Haskell.LSP.Types.Capabilities (ClientCapabilities)

#if defined(GHC_LIB)
import Development.IDE.Import.DependencyInformation
#endif
nonLocalCompletionsDescriptor :: PluginId -> PluginDescriptor IdeState
nonLocalCompletionsDescriptor plId =
  (defaultPluginDescriptor plId)
    { pluginCompletionProvider = Just $ completionsProvider
        (useWithStaleFast NonLocalCompletions)
        (const $ return (mempty, zeroMapping)),
      pluginRules = nonLocalCompletionsRule
    }

localCompletionsDescriptor :: PluginId -> PluginDescriptor IdeState
localCompletionsDescriptor plId =
  (defaultPluginDescriptor plId)
    { pluginCompletionProvider = Just $ completionsProvider
        (useWithStaleFast LocalCompletions)
        (fmap (fromMaybe (mempty, zeroMapping)) . useWithStaleFast GetBindings),
      pluginRules = localCompletionsRule
    }

keywordCompletionsDescriptor :: PluginId -> PluginDescriptor IdeState
keywordCompletionsDescriptor plId =
  (defaultPluginDescriptor plId)
    { pluginCompletionProvider = Just $ pureCompletionsProvider (\ideOpts pFix _ _ -> getKeywordCompletions ideOpts pFix)
    }

-- | This completions provider covers all the pragmas except for LANGUAGE pragmas,
--   which are already covered by:
--  https://github.com/haskell/haskell-language-server/blob/master/plugins/default/src/Ide/Plugin/Pragmas.hs
pragmaCompletionsDescriptor :: PluginId -> PluginDescriptor IdeState
pragmaCompletionsDescriptor plId =
  (defaultPluginDescriptor plId)
    { pluginCompletionProvider = Just $ pureCompletionsProvider (\_ -> getOptionCompletions)
    }

--------------------------------------------------------------------------------
localCompletionsRule :: Rules ()
localCompletionsRule = define $ \LocalCompletions file -> do
        pm <- useWithStale GetParsedModule file
        case pm of
            Just (pm, _) -> do
                let cdata = localCompletionsForParsedModule pm
                return ([], Just cdata)
            _ -> return ([], Nothing)
nonLocalCompletionsRule :: Rules ()
nonLocalCompletionsRule = define $ \NonLocalCompletions file -> do
        -- For non local completions we avoid depending on the parsed module,
        -- synthetizing a fake module with an empty body from the buffer
        -- in the ModSummary, which preserves all the imports
        ms <- fmap fst <$> useWithStale GetModSummaryWithoutTimestamps file
        sess <- fmap fst <$> useWithStale GhcSessionDeps file

-- When possible, rely on the haddocks embedded in our interface files
-- This creates problems on ghc-lib, see comment on 'getDocumentationTryGhc'
#if !defined(GHC_LIB)
        let parsedDeps = []
#else
        deps <- maybe (TransitiveDependencies [] [] []) fst <$> useWithStale GetDependencies file
        parsedDeps <- mapMaybe (fmap fst) <$> usesWithStale GetParsedModule (transitiveModuleDeps deps)
#endif

        case (ms, sess) of
            (Just (ms,imps), Just sess) -> do
              let env = hscEnv sess
              -- We do this to be able to provide completions of items that are not restricted to the explicit list
              res <- liftIO $ tcRnImportDecls env (dropListFromImportDecl <$> imps)
              case res of
                  (_, Just rdrEnv) -> do
                      cdata <- liftIO $ cacheDataProducer env (ms_mod ms) rdrEnv imps parsedDeps
                      return ([], Just cdata)
                  (_diag, _) ->
                      return ([], Nothing)
            _ -> return ([], Nothing)

-- Drop any explicit imports in ImportDecl if not hidden
dropListFromImportDecl :: GenLocated SrcSpan (ImportDecl GhcPs) -> GenLocated SrcSpan (ImportDecl GhcPs)
dropListFromImportDecl iDecl = let
    f d@ImportDecl {ideclHiding} = case ideclHiding of
        Just (False, _) -> d {ideclHiding=Nothing}
        -- if hiding or Nothing just return d
        _ -> d
    f x = x
    in f <$> iDecl

-- | Produce completions info for a file
type instance RuleResult LocalCompletions = CachedCompletions
type instance RuleResult NonLocalCompletions = CachedCompletions

data LocalCompletions = LocalCompletions
    deriving (Eq, Show, Typeable, Generic)
instance Hashable LocalCompletions
instance NFData   LocalCompletions
instance Binary   LocalCompletions

data NonLocalCompletions = NonLocalCompletions
    deriving (Eq, Show, Typeable, Generic)
instance Hashable NonLocalCompletions
instance NFData   NonLocalCompletions
instance Binary   NonLocalCompletions

completionsProvider ::
  (NormalizedFilePath -> IdeAction (Maybe (CachedCompletions, a))) ->
  (NormalizedFilePath -> IdeAction (Bindings, PositionMapping)) ->
  LSP.LspFuncs Config
    -> IdeState
    -> CompletionParams
    -> IO (Either ResponseError CompletionResponseResult)
completionsProvider getCached getLocal lsp ide
  CompletionParams{_textDocument=TextDocumentIdentifier uri
                  ,_position=position
                  ,_context=completionContext} = do
    contents <- LSP.getVirtualFileFunc lsp $ toNormalizedUri uri
    fmap Right $ case (contents, uriToFilePath' uri) of
      (Just cnts, Just path) -> do
        let npath = toNormalizedFilePath' path
        (ideOpts, compls) <- runIdeAction "Completion" (shakeExtras ide) $ do
            opts <- liftIO $ getIdeOptionsIO $ shakeExtras ide
            compls <- getCached npath
            pm <- useWithStaleFast GetParsedModule npath
            binds <- getLocal npath
            pure (opts, fmap (,pm,binds) compls )
        case compls of
          Just ((cci', _), parsedMod, bindMap) -> do
            pfix <- VFS.getCompletionPrefix position cnts
            case (pfix, completionContext) of
              (Just (VFS.PosPrefixInfo _ "" _ _), Just CompletionContext { _triggerCharacter = Just "."})
                -> return (Completions $ List [])
              (Just pfix', _) -> do
                let clientCaps = clientCapabilities $ shakeExtras ide
                config <- getClientConfig lsp
                let snippets = WithSnippets . completionSnippetsOn $ config
                allCompletions <- getCompletions ideOpts cci' parsedMod bindMap pfix' clientCaps snippets
                pure $ Completions (List allCompletions)
              _ -> return (Completions $ List [])
          _ -> return (Completions $ List [])
      _ -> return (Completions $ List [])

pureCompletionsProvider ::
  ( IdeOptions ->
    VFS.PosPrefixInfo ->
    ClientCapabilities ->
    WithSnippets ->
    [CompletionItem]
  ) ->
  LSP.LspFuncs Config ->
  IdeState ->
  CompletionParams ->
  IO (Either ResponseError CompletionResponseResult)
pureCompletionsProvider
  getCompletions
  lsp
  ide
  CompletionParams
    { _textDocument = TextDocumentIdentifier uri,
      _position = position,
      _context = completionContext
    } = do
    ideOptions <- liftIO $ getIdeOptionsIO $ shakeExtras ide
    snippets <- WithSnippets . completionSnippetsOn <$> getClientConfig lsp
    cnts <- LSP.getVirtualFileFunc lsp $ toNormalizedUri uri
    pfix <- VFS.getCompletionPrefix position `traverse` cnts
    let clientCaps = clientCapabilities $ shakeExtras ide
    Right <$> case (join pfix, completionContext) of
      (Just (VFS.PosPrefixInfo _ "" _ _), Just CompletionContext {_triggerCharacter = Just "."}) ->
        return (Completions $ List [])
      (Just pfix', _) ->
        return $ Completions $ List $ getCompletions ideOptions pfix' clientCaps snippets
      _ -> return (Completions $ List [])
