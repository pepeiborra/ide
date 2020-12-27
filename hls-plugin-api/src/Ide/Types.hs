{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ide.Types
    (
      IdePlugins(..)
    , PluginDescriptor(..)
    , defaultPluginDescriptor
    , PluginCommand(..)
    , PluginId(..)
    , CommandId(..)
    , mkLspCmdId
    , mkLspCommand
    , responseError
    , DiagnosticProvider(..)
    , DiagnosticProviderFunc(..)
    , SymbolsProvider
    , FormattingType(..)
    , FormattingProvider
    , noneProvider
    , HoverProvider
    , CodeActionProvider
    , CodeLensProvider
    , CommandFunction
    , ExecuteCommandProvider
    , CompletionProvider
    , RenameProvider
    , WithSnippets(..)
    , getProcessID
    , getClientConfig
    , getPluginConfig
    , configForPlugin
    , pluginEnabled
    ) where

import           Data.Aeson                    hiding (defaultOptions)
import qualified Data.Default
import qualified Data.Map  as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Set                      as S
import           Data.String
import qualified Data.Text                     as T
import           Development.Shake
import           Ide.Plugin.Config
import qualified Language.Haskell.LSP.Core as LSP
import           Language.Haskell.LSP.Types
import           Text.Regex.TDFA.Text()
import qualified Data.Aeson.Types as J

#ifdef mingw32_HOST_OS
import qualified System.Win32.Process as P (getCurrentProcessId)
#else
import qualified System.Posix.Process as P (getProcessID)
#endif

-- ---------------------------------------------------------------------

newtype IdePlugins ideState = IdePlugins
  { ipMap :: Map.Map PluginId (PluginDescriptor ideState)}

-- ---------------------------------------------------------------------

data PluginDescriptor ideState =
  PluginDescriptor { pluginId                 :: !PluginId
                   , pluginRules              :: !(Rules ())
                   , pluginCommands           :: ![PluginCommand ideState]
                   , pluginCodeActionProvider :: !(Maybe (CodeActionProvider ideState))
                   , pluginCodeLensProvider   :: !(Maybe (CodeLensProvider ideState))
                   , pluginDiagnosticProvider :: !(Maybe DiagnosticProvider)
                     -- ^ TODO: diagnostics are generally provided via rules,
                     -- this is probably redundant.
                   , pluginHoverProvider      :: !(Maybe (HoverProvider ideState))
                   , pluginSymbolsProvider    :: !(Maybe (SymbolsProvider ideState))
                   , pluginFormattingProvider :: !(Maybe (FormattingProvider ideState IO))
                   , pluginCompletionProvider :: !(Maybe (CompletionProvider ideState))
                   , pluginRenameProvider     :: !(Maybe (RenameProvider ideState))
                   }

defaultPluginDescriptor :: PluginId -> PluginDescriptor ideState
defaultPluginDescriptor plId =
  PluginDescriptor
    plId
    mempty
    mempty
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing

-- instance Show PluginCommand where
--   show (PluginCommand i _ _) = "PluginCommand { name = " ++ show i ++ " }"

-- newtype CommandId = CommandId T.Text
--   deriving (Show, Read, Eq, Ord)
-- instance IsString CommandId where
--   fromString = CommandId . T.pack

-- data PluginCommand = forall a b. (FromJSON a, ToJSON b, Typeable b) =>
--   PluginCommand { commandId   :: CommandId
--                 , commandDesc :: T.Text
--                 , commandFunc :: a -> IO (Either ResponseError b)
--                 }

newtype CommandId = CommandId T.Text
  deriving (Show, Read, Eq, Ord)
instance IsString CommandId where
  fromString = CommandId . T.pack

data PluginCommand ideState = forall a. (FromJSON a) =>
  PluginCommand { commandId   :: CommandId
                , commandDesc :: T.Text
                , commandFunc :: CommandFunction ideState a
                }

mkLspCommand :: PluginId -> CommandId -> T.Text -> Maybe [J.Value] -> IO Command
mkLspCommand plid cn title args' = do
  pid <- getPid
  let cmdId = mkLspCmdId pid plid cn
  let args = List <$> args'
  return $ Command title cmdId args

mkLspCmdId :: T.Text -> PluginId -> CommandId -> T.Text
mkLspCmdId pid (PluginId plid) (CommandId cid)
  = pid <> ":" <> plid <> ":" <> cid

-- | Get the operating system process id for the running server
-- instance. This should be the same for the lifetime of the instance,
-- and different from that of any other currently running instance.
getPid :: IO T.Text
getPid = T.pack . show <$> getProcessID

getProcessID :: IO Int
#ifdef mingw32_HOST_OS
getProcessID = fromIntegral <$> P.getCurrentProcessId
#else
getProcessID = fromIntegral <$> P.getProcessID
#endif


-- ---------------------------------------------------------------------

type CommandFunction ideState a = LSP.LspFuncs Config
                       -> ideState
                       -> a
                       -> IO (Either ResponseError Value, Maybe (ServerMethod, ApplyWorkspaceEditParams))

type CodeActionProvider ideState = LSP.LspFuncs Config
                        -> ideState
                        -> PluginId
                        -> TextDocumentIdentifier
                        -> Range
                        -> CodeActionContext
                        -> IO (Either ResponseError (List CAResult))

type CompletionProvider ideState = LSP.LspFuncs Config
                        -> ideState
                        -> CompletionParams
                        -> IO (Either ResponseError CompletionResponseResult)



type CodeLensProvider ideState = LSP.LspFuncs Config
                      -> ideState
                      -> PluginId
                      -> CodeLensParams
                      -> IO (Either ResponseError (List CodeLens))

type RenameProvider ideState = LSP.LspFuncs Config
                    -> ideState
                    -> RenameParams
                    -> IO (Either ResponseError WorkspaceEdit)

type DiagnosticProviderFuncSync
  = DiagnosticTrigger -> Uri
  -> IO (Either ResponseError (Map.Map Uri (S.Set Diagnostic)))

type DiagnosticProviderFuncAsync
  = DiagnosticTrigger -> Uri
  -> (Map.Map Uri (S.Set Diagnostic) -> IO ())
  -> IO (Either ResponseError ())

data DiagnosticProviderFunc
  = DiagnosticProviderSync  DiagnosticProviderFuncSync
  | DiagnosticProviderAsync DiagnosticProviderFuncAsync


data DiagnosticProvider = DiagnosticProvider
     { dpTrigger :: S.Set DiagnosticTrigger -- AZ:should this be a NonEmptyList?
     , dpFunc    :: DiagnosticProviderFunc
     }

data DiagnosticTrigger = DiagnosticOnOpen
                       | DiagnosticOnChange
                       | DiagnosticOnSave
                       deriving (Show,Ord,Eq)

-- type HoverProvider = Uri -> Position -> IO (Either ResponseError [Hover])
type HoverProvider ideState = ideState -> TextDocumentPositionParams -> IO (Either ResponseError (Maybe Hover))

type SymbolsProvider ideState = LSP.LspFuncs Config
                     -> ideState
                     -> DocumentSymbolParams
                     -> IO (Either ResponseError [DocumentSymbol])

type ExecuteCommandProvider ideState = ideState
                            -> ExecuteCommandParams
                            -> IO (Either ResponseError Value, Maybe (ServerMethod, ApplyWorkspaceEditParams))

newtype WithSnippets = WithSnippets Bool

-- ---------------------------------------------------------------------

newtype PluginId = PluginId T.Text
  deriving (Show, Read, Eq, Ord)
instance IsString PluginId where
  fromString = PluginId . T.pack

-- ---------------------------------------------------------------------


-- | Format the given Text as a whole or only a @Range@ of it.
-- Range must be relative to the text to format.
-- To format the whole document, read the Text from the file and use 'FormatText'
-- as the FormattingType.
data FormattingType = FormatText
                    | FormatRange Range


-- | To format a whole document, the 'FormatText' @FormattingType@ can be used.
-- It is required to pass in the whole Document Text for that to happen, an empty text
-- and file uri, does not suffice.
type FormattingProvider ideState m
        = LSP.LspFuncs Config
        -> ideState
        -> FormattingType  -- ^ How much to format
        -> T.Text -- ^ Text to format
        -> NormalizedFilePath -- ^ location of the file being formatted
        -> FormattingOptions -- ^ Options for the formatter
        -> m (Either ResponseError (List TextEdit)) -- ^ Result of the formatting

noneProvider :: FormattingProvider ideState IO
noneProvider _ _ _ _ _ _ = return $ Right (List [])

-- ---------------------------------------------------------------------

responseError :: T.Text -> ResponseError
responseError txt = ResponseError InvalidParams txt Nothing


-- ---------------------------------------------------------------------
-- | Returns the current client configuration. It is not wise to permanently
-- cache the returned value of this function, as clients can at runitime change
-- their configuration.
--
-- If no custom configuration has been set by the client, this function returns
-- our own defaults.
getClientConfig :: LSP.LspFuncs Config -> IO Config
getClientConfig lf = fromMaybe Data.Default.def <$> LSP.config lf

-- ---------------------------------------------------------------------

-- | Returns the current plugin configuration. It is not wise to permanently
-- cache the returned value of this function, as clients can change their
-- configuration at runtime.
--
-- If no custom configuration has been set by the client, this function returns
-- our own defaults.
getPluginConfig :: LSP.LspFuncs Config -> PluginId -> IO PluginConfig
getPluginConfig lf plugin = do
    config <- getClientConfig lf
    return $ configForPlugin config plugin

configForPlugin :: Config -> PluginId -> PluginConfig
configForPlugin config (PluginId plugin)
    = Map.findWithDefault Data.Default.def plugin (plugins config)

-- ---------------------------------------------------------------------

-- | Checks that a given plugin is both enabled and the specific feature is
-- enabled
pluginEnabled :: PluginConfig -> (PluginConfig -> Bool) -> Bool
pluginEnabled pluginConfig f = plcGlobalOn pluginConfig && f pluginConfig
