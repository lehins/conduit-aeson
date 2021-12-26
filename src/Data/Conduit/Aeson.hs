{-# LANGUAGE LambdaCase #-}
-- |
-- Module      : Data.Conduit.Aeson
-- Copyright   : (c) Alexey Kuleshevich 2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Conduit.Aeson
  ( ParserError(..)
  , conduitArray
  , conduitArrayEither
  , conduitObject
  , conduitObjectEither
  -- * Helpers
  , conduitArrayParserEither
  , conduitArrayParserNoStartEither
  , conduitObjectParserEither
  , conduitObjectParserNoStartEither
  ) where

import Conduit
import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Aeson as Aeson
import qualified Data.Aeson.Parser as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Attoparsec.ByteString as Atto
import qualified Data.Attoparsec.ByteString.Char8 as Atto8
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import Data.Coerce
import Data.Conduit.Attoparsec
import qualified Data.Text as T

data ParserError
  = AttoParserError ParseError
  | AesonParserError String
  | NotTerminatedInput
  -- ^ Raised when reached end of stream without encountering expected closing
  -- bracket.
  deriving Show
instance Exception ParserError


-- | Parse a top level object into a stream of key/value pairs. Throws
-- `ParserError` on invalid input, see `conduitObjectEither` for more graceful
-- error handling.
--
-- @since 0.1.0
conduitObject ::
     (FromJSONKey k, FromJSON v, MonadThrow m) => ConduitM BS.ByteString (k, v) m ()
conduitObject = conduitObjectEither .| mapMC (either throwM pure)

-- | Parse a top level object into a stream of key/value pairs with potential
-- failures as `ParserError`.
--
-- @since 0.1.0
conduitObjectEither ::
     (FromJSONKey k, FromJSON v, Monad m)
  => ConduitM BS.ByteString (Either ParserError (k, v)) m ()
conduitObjectEither = conduitObjectParserEither .| stopOnNothing .| mapC toKeyValue
  where
    toKeyValue (Left err) = Left err
    toKeyValue (Right (_, (k, v))) =
      first AesonParserError $ do
        key <-
          case fromJSONKey of
            FromJSONKeyCoerce       -> Right $ coerce k
            FromJSONKeyText f       -> Right $ f k
            FromJSONKeyTextParser p -> Aeson.parseEither p k
            FromJSONKeyValue p      -> Aeson.parseEither p (String k)
        val <- Aeson.parseEither Aeson.parseJSON v
        Right (key, val)

-- | Parse a top level key value mapping. Expects opening and closing braces @{}@
--
-- @since 0.1.0
conduitObjectParserEither ::
     Monad m
  => ConduitM  BS.ByteString (Either ParseError (PositionRange, Maybe (T.Text, Value))) m ()
conduitObjectParserEither = do
  sinkParserEither objectEntryPrefixParser >>= \case
    Left err -> yield $ Left err
    Right () -> conduitObjectParserNoStartEither

-- | Expects that there is no opening of a top level object curly brace @{@, but
-- will stop as soon as the closing one @}@ is reached.
--
-- @since 0.1.0
conduitObjectParserNoStartEither ::
     Monad m
  => ConduitM BS.ByteString (Either ParseError (PositionRange, Maybe (T.Text, Value))) m ()
conduitObjectParserNoStartEither = conduitParserEither objectEntryParser



-- | Parse a top level array into a stream of values.
--
-- @since 0.1.0
conduitArray ::
     (FromJSON v, MonadThrow m) => ConduitM BS.ByteString v m ()
conduitArray = conduitArrayEither .| mapMC (either throwM pure)

conduitArrayEither ::
     (FromJSON v, Monad m)
  => ConduitM BS.ByteString (Either ParserError v) m ()
conduitArrayEither = conduitArrayParserEither .| stopOnNothing .| mapC toValue
  where
    toValue (Left err) = Left err
    toValue (Right (_, v)) = first AesonParserError $ Aeson.parseEither Aeson.parseJSON v

-- | Parse a top level key value mapping. Expects both opening and closing braces @[]@
conduitArrayParserEither ::
     Monad m
  => ConduitM  BS.ByteString (Either ParseError (PositionRange, Maybe Value)) m ()
conduitArrayParserEither = do
  sinkParserEither valuePrefixParser >>= \case
    Left err -> yield $ Left err
    Right () -> conduitArrayParserNoStartEither

-- | Expects that there is no opening of a top level array brace @[@, but will
-- stop as soon as the closing one @]@ is reached, possibly leaving behind
-- unconsumed input.
conduitArrayParserNoStartEither ::
     Monad m
  => ConduitM BS.ByteString (Either ParseError (PositionRange, Maybe Value)) m ()
conduitArrayParserNoStartEither = conduitParserEither valueParser

stopOnNothing ::
     Monad m
  => ConduitM (Either ParseError (PositionRange, Maybe a))
              (Either ParserError (PositionRange, a)) m ()
stopOnNothing = do
  await >>= \case
    Nothing -> yield $ Left NotTerminatedInput
    Just e
      | Left err <- e -> yield (Left (AttoParserError err)) >> stopOnNothing
      | Right (p, Just r) <- e -> yield (Right (p, r)) >> stopOnNothing
      | Right (_, Nothing) <- e -> pure ()

-- Attoparsec

delimitedParser :: Char -> Char -> Atto.Parser Aeson.Value
delimitedParser d t =
  json' <* skipSpace <* (void (Atto8.char8 d) <|> expectTermination)
  where
    expectTermination =
      Atto8.peekChar >>= \case
        Just c
          | c /= t -> fail $ "Unexpected delimiter: " <> show c
        _ -> pure ()


valueParser :: Atto.Parser (Maybe Aeson.Value)
valueParser =
  skipSpace *>
  ((Nothing <$ Atto8.char ']') <|> (Just <$> delimitedParser ',' ']'))

valuePrefixParser :: Atto.Parser ()
valuePrefixParser = skipSpace <* Atto8.char '[' <* skipSpace

objectEntryParser :: Atto.Parser (Maybe (T.Text, Aeson.Value))
objectEntryParser =
  skipSpace *>
  ((Nothing <$ Atto8.char '}') <|> do
     k <-
       (Aeson.jstring Atto.<?> "key") <* skipSpace <*
       (Atto8.char ':' Atto.<?> "':'")
     v <- skipSpace *> delimitedParser ',' '}'
     pure $ Just (k, v))

objectEntryPrefixParser :: Atto.Parser ()
objectEntryPrefixParser = skipSpace <* Atto8.char '{' <* skipSpace

skipSpace :: Atto.Parser ()
skipSpace = Atto.skipWhile $ \w -> w == 0x20 || w == 0x0a || w == 0x0d || w == 0x09
