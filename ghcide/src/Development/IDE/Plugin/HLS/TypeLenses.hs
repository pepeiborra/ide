-- | An HLS plugin to provide code lenses for type signatures
module Development.IDE.Plugin.HLS.TypeLenses
  ( descriptor,
    suggestSignature,
    typeLensCommandId,
  )
where

import Data.Aeson.Types (Result (..), Value (..), fromJSON, toJSON)
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T
import Development.IDE.Core.RuleTypes (TypeCheck (TypeCheck))
import Development.IDE.Core.Rules (IdeState, runAction)
import Development.IDE.Core.Service (getDiagnostics)
import Development.IDE.Core.Shake (getHiddenDiagnostics, use)
import Development.IDE.Types.Location
  ( Position (Position, _character, _line),
    Range (Range, _end, _start),
    toNormalizedFilePath',
    uriToFilePath',
  )
import Ide.Types
  ( CommandFunction,
    CommandId (CommandId),
    PluginCommand (PluginCommand),
    PluginDescriptor (pluginCodeLensProvider, pluginCommands),
    PluginId,
    defaultPluginDescriptor,
  )
import qualified Language.Haskell.LSP.Core as LSP
import Language.Haskell.LSP.Types
  ( ApplyWorkspaceEditParams (ApplyWorkspaceEditParams),
    CodeLens (CodeLens),
    CodeLensParams (CodeLensParams, _textDocument),
    Diagnostic (..),
    ExecuteCommandParams (..),
    List (..),
    ResponseError,
    ServerMethod (WorkspaceApplyEdit),
    TextDocumentIdentifier (TextDocumentIdentifier),
    TextEdit (TextEdit),
    WorkspaceEdit (WorkspaceEdit),
  )
import Text.Regex.TDFA ((=~))
import Ide.PluginUtils (mkLspCommand)

typeLensCommandId :: T.Text
typeLensCommandId = "typesignature.add"
descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId =
  (defaultPluginDescriptor plId)
    { pluginCodeLensProvider = Just codeLensProvider,
      pluginCommands = [PluginCommand (CommandId typeLensCommandId) "adds a signature" commandAddSignature]
    }

typeSignatureCommandId :: CommandId
typeSignatureCommandId = CommandId "typesignature.add"

codeLensProvider ::
  LSP.LspFuncs c ->
  IdeState ->
  PluginId ->
  CodeLensParams ->
  IO (Either ResponseError (List CodeLens))
codeLensProvider _lsp ideState pId CodeLensParams {_textDocument = TextDocumentIdentifier uri} = do
  fmap (Right . List) $ case uriToFilePath' uri of
    Just (toNormalizedFilePath' -> filePath) -> do
      _ <- runAction "codeLens" ideState (use TypeCheck filePath)
      diag <- getDiagnostics ideState
      hDiag <- getHiddenDiagnostics ideState
      sequence
        [ generateLens pId _range title (toJSON edit)
          | (dFile, _, dDiag@Diagnostic {_range = _range}) <- diag ++ hDiag,
            dFile == filePath,
            (title, tedit) <- suggestSignature False dDiag,
            let edit = WorkspaceEdit (Just $ Map.singleton uri $ List tedit) Nothing
        ]
    Nothing -> pure []

generateLens :: PluginId -> Range -> T.Text -> Value -> IO CodeLens
generateLens pId _range title edit = do
  cId <- mkLspCommand pId typeSignatureCommandId title (Just [edit])
  return $ CodeLens _range (Just cId) Nothing
commandAddSignature :: CommandFunction IdeState WorkspaceEdit
commandAddSignature lf ide params =
  commandHandler lf ide (ExecuteCommandParams "typesignature.add" (Just (List [toJSON params])) Nothing)

-- | Execute the "typesignature.add" command.
commandHandler ::
  LSP.LspFuncs c ->
  IdeState ->
  ExecuteCommandParams ->
  IO (Either ResponseError Value, Maybe (ServerMethod, ApplyWorkspaceEditParams))
commandHandler _lsp _ideState ExecuteCommandParams {..}
  | Just (List [edit]) <- _arguments,
    Success wedit <- fromJSON edit =
    return (Right Null, Just (WorkspaceApplyEdit, ApplyWorkspaceEditParams wedit))
  | otherwise =
    return (Right Null, Nothing)

suggestSignature :: Bool -> Diagnostic -> [(T.Text, [TextEdit])]
suggestSignature isQuickFix Diagnostic {_range = _range@Range {..}, ..}
  | _message
      =~ ("(Top-level binding|Polymorphic local binding|Pattern synonym) with no type signature" :: T.Text) =
    let signature =
          removeInitialForAll $
            T.takeWhile (\x -> x /= '*' && x /= '•') $
              T.strip $ unifySpaces $ last $ T.splitOn "type signature: " $ filterNewlines _message
        startOfLine = Position (_line _start) startCharacter
        beforeLine = Range startOfLine startOfLine
        title = if isQuickFix then "add signature: " <> signature else signature
        action = TextEdit beforeLine $ signature <> "\n" <> T.replicate startCharacter " "
     in [(title, [action])]
  where
    removeInitialForAll :: T.Text -> T.Text
    removeInitialForAll (T.breakOnEnd " :: " -> (nm, ty))
      | "forall" `T.isPrefixOf` ty = nm <> T.drop 2 (snd (T.breakOn "." ty))
      | otherwise = nm <> ty
    startCharacter
      | "Polymorphic local binding" `T.isPrefixOf` _message =
        _character _start
      | otherwise =
        0
suggestSignature _ _ = []

-- TODO deduplicate these functions with the CodeActions module
unifySpaces :: T.Text -> T.Text
unifySpaces = T.unwords . T.words

filterNewlines :: T.Text -> T.Text
filterNewlines = T.concat . T.lines
