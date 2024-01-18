{-# LANGUAGE FlexibleContexts #-}

module FlatBuffers.Internal.Compiler.ParserIO where

import Control.Monad (when)
import Control.Monad.Except (MonadError, MonadIO, liftIO, throwError)
import Control.Monad.State (MonadState, execStateT, get, put)

import Data.Coerce (coerce)
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

import FlatBuffers.Internal.Compiler.Display (display)
import FlatBuffers.Internal.Compiler.Parser (schema)
import FlatBuffers.Internal.Compiler.SyntaxTree
  (FileTree(..), Include(..), Schema, StringLiteral(..), includes)

import System.Directory qualified as Dir
import System.FilePath qualified as FP

import Text.Megaparsec (errorBundlePretty, parse)

parseSchemas ::
     MonadIO m
  => MonadError String m
  => FilePath -- ^ Filepath of the root schema. It must be a path relative to the project root or an absolute path.
  -> [FilePath] -- ^ Directories to search for @include@s.
  -> m (FileTree Schema)
parseSchemas rootFilePath includeDirs = do
  fileContent <- liftIO $ readFile rootFilePath
  case parse schema rootFilePath fileContent of
    Left err -> throwError $ errorBundlePretty err
    Right rootSchema -> do
      rootFilePathCanon <- liftIO $ Dir.canonicalizePath rootFilePath
      let importedFilePaths = T.unpack . coerce <$> includes rootSchema

      importedSchemas <- flip execStateT Map.empty $
                            traverse_
                              (parseImportedSchema includeDirs rootFilePathCanon)
                              importedFilePaths
      pure FileTree
            { fileTreeFilePath = rootFilePathCanon
            , fileTreeRoot     = rootSchema
            , fileTreeForest   = importedSchemas
            }

parseImportedSchema ::
     MonadState (Map FilePath Schema) m
  => MonadIO m
  => MonadError String m
  => [FilePath]
  -> FilePath
  -> FilePath
  -> m ()
parseImportedSchema includeDirs rootFilePathCanon filePath =
  go rootFilePathCanon filePath
  where
    go parentSchemaPath filePath = do

      let parentSchemaDir = FP.takeDirectory parentSchemaPath
      let dirCandidates = parentSchemaDir : includeDirs

      actualFilePathCanonMaybe <- liftIO $ Dir.findFile dirCandidates filePath >>= traverse Dir.canonicalizePath

      case actualFilePathCanonMaybe of
        Nothing -> throwError $
          "File '"
          <> filePath
          <> "' (imported from '"
          <> parentSchemaPath
          <> "') not found.\nSearched in these directories: "
          <> display dirCandidates
        Just actualFilePathCanon -> do
          importedSchemas <- get
          when (actualFilePathCanon /= rootFilePathCanon && actualFilePathCanon `Map.notMember` importedSchemas) $ do
            fileContent <- liftIO $ readFile actualFilePathCanon
            case parse schema actualFilePathCanon fileContent of
              Left err -> throwError $ errorBundlePretty err
              Right importedSchema -> do
                put (Map.insert actualFilePathCanon importedSchema importedSchemas)
                traverse_ (go actualFilePathCanon . T.unpack . coerce) (includes importedSchema)
