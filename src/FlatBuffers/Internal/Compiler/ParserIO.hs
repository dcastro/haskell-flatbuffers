{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FlatBuffers.Internal.Compiler.ParserIO where

import           Control.Monad                            ( when )
import           Control.Monad.Except                     ( MonadError, MonadIO, liftIO, throwError )
import           Control.Monad.State                      ( MonadState, execStateT, get, put )

import           Data.Coerce                              ( coerce )
import           Data.Foldable                            ( traverse_ )
import qualified Data.List                                as List
import qualified Data.List.NonEmpty                       as NE
import           Data.List.NonEmpty                       ( NonEmpty(..) )
import           Data.Map.Strict                          ( Map )
import qualified Data.Map.Strict                          as Map
import           Data.Maybe                               ( isNothing )
import           Data.Proxy                               ( Proxy(..) )
import qualified Data.Set                                 as E
import           Data.Set                                 ( Set )
import           Data.Text                                ( Text )
import qualified Data.Text                                as T

import           FlatBuffers.Internal.Compiler.Display    ( display )
import           FlatBuffers.Internal.Compiler.Parser     ( schema )
import           FlatBuffers.Internal.Compiler.SyntaxTree ( FileTree(..), Include(..), Schema, StringLiteral(..), includes )

import qualified System.Directory                         as Dir
import qualified System.FilePath                          as FP

import           Text.Megaparsec                          hiding ( errorBundlePretty, parseErrorTextPretty )
import qualified Text.Megaparsec.Stream                   as Stream

parseSchemas ::
     MonadIO m
  => MonadError Text m
  => FilePath -- ^ Filepath of the root schema. It must be a path relative to the project root or an absolute path.
  -> [FilePath] -- ^ Directories to search for @include@s.
  -> m (FileTree Schema)
parseSchemas rootFilePath includeDirs = do
  fileContent <- liftIO $ readFile rootFilePath
  case parse schema rootFilePath fileContent of
    Left err -> throwError . T.pack $ errorBundlePretty err
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
  => MonadError Text m
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
          <> T.pack filePath
          <> "' (imported from '"
          <> T.pack parentSchemaPath
          <> "') not found.\n Searched in these directories: ["
          <> display (T.pack <$> dirCandidates)
          <> "]"
        Just actualFilePathCanon -> do
          importedSchemas <- get
          when (actualFilePathCanon /= rootFilePathCanon && actualFilePathCanon `Map.notMember` importedSchemas) $ do
            fileContent <- liftIO $ readFile actualFilePathCanon
            case parse schema actualFilePathCanon fileContent of
              Left err -> throwError . T.pack $ errorBundlePretty err
              Right importedSchema -> do
                put (Map.insert actualFilePathCanon importedSchema importedSchemas)
                traverse_ (go actualFilePathCanon . T.unpack . coerce) (includes importedSchema)

errorBundlePretty
  :: forall s e. ( Stream s
                 , ShowErrorComponent e
                 )
  => ParseErrorBundle s e -- ^ Parse error bundle to display
  -> String               -- ^ Textual rendition of the bundle
errorBundlePretty ParseErrorBundle {..} =
  let (r, _) = foldl f (id, bundlePosState) bundleErrors
  in drop 1 (r "")
  where
    f :: (ShowS, PosState s)
      -> ParseError s e
      -> (ShowS, PosState s)
    f (o, !pst) e = (o . (outChunk ++), pst')
      where
        (epos, sline, pst') = Stream.reachOffset (errorOffset e) pst
        outChunk =
          "\n" <> sourcePosPretty epos <> ":\n" <>
          padding <> "|\n" <>
          lineNumber <> " | " <> sline <> "\n" <>
          padding <> "| " <> rpadding <> pointer <> "\n" <>
          parseErrorTextPretty e
        lineNumber = (show . unPos . sourceLine) epos
        padding = replicate (length lineNumber + 1) ' '
        rpadding =
          if pointerLen > 0
            then replicate rpshift ' '
            else ""
        rpshift = unPos (sourceColumn epos) - 1
        pointer = replicate pointerLen '^'
        pointerLen =
          if rpshift + elen > slineLen
            then slineLen - rpshift + 1
            else elen
        slineLen = length sline
        elen =
          case e of
            TrivialError _ Nothing _ -> 1
            TrivialError _ (Just x) _ -> errorItemLength x
            FancyError _ xs ->
              E.foldl' (\a b -> max a (errorFancyLength b)) 1 xs


parseErrorTextPretty
  :: forall s e. (Stream s, ShowErrorComponent e)
  => ParseError s e    -- ^ Parse error to render
  -> String            -- ^ Result of rendering
parseErrorTextPretty (TrivialError _ us ps) =
  if isNothing us && E.null ps
    then "unknown parse error\n"
    else messageItemsPretty "unexpected " (showErrorItem pxy `E.map` maybe E.empty E.singleton us) <>
         messageItemsPretty "expecting "  (showErrorItem pxy `E.map` ps)
  where
    pxy = Proxy :: Proxy s
parseErrorTextPretty (FancyError _ xs) =
  if E.null xs
    then "unknown fancy parse error\n"
    else unlines (showErrorFancy <$> E.toAscList xs)

showErrorFancy :: ShowErrorComponent e => ErrorFancy e -> String
showErrorFancy = \case
  ErrorFail msg -> msg
  ErrorIndentation ord ref actual ->
    "incorrect indentation (got " <> show (unPos actual) <>
    ", should be " <> p <> show (unPos ref) <> ")"
    where
      p = case ord of
            LT -> "less than "
            EQ -> "equal to "
            GT -> "greater than "
  ErrorCustom a -> showErrorComponent a

showErrorItem :: Stream s => Proxy s -> ErrorItem (Token s) -> String
showErrorItem pxy = \case
    Tokens   ts -> showTokens pxy ts
    Label label -> NE.toList label
    EndOfInput  -> "end of input"

messageItemsPretty
  :: String            -- ^ Prefix to prepend
  -> Set String        -- ^ Collection of messages
  -> String            -- ^ Result of rendering
messageItemsPretty prefix ts
  | E.null ts = ""
  | otherwise =
    prefix <> (orList . NE.fromList . E.toAscList) ts <> "\n"

orList :: NonEmpty String -> String
orList (x:|[])  = x
orList (x:|[y]) = x <> " or " <> y
orList xs       = List.intercalate ", " (NE.init xs) <> ", or " <> NE.last xs

errorFancyLength :: ShowErrorComponent e => ErrorFancy e -> Int
errorFancyLength = \case
  ErrorCustom a -> errorComponentLen a
  _             -> 1

errorItemLength :: ErrorItem t -> Int
errorItemLength = \case
  Tokens ts -> NE.length ts
  _         -> 1

