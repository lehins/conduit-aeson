{-# LANGUAGE CPP #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase #-}
-- |
-- Module      : Data.Conduit.Aeson
-- Copyright   : (c) Alexey Kuleshevich 2021-2022
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
  -- ** Conduit
  , conduitArrayParserEither
  , conduitArrayParserNoStartEither
  , conduitObjectParserEither
  , conduitObjectParserNoStartEither
  -- ** Attoparsec
  , skipSpace
  , commaParser
  , delimiterParser
  , valuePrefixParser
  , valueParser
  , objectEntryPrefixParser
  , objectEntryParser
  , objectEntryMaybeParser
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
import Data.Conduit.Attoparsec
import qualified Data.Text as T
#if MIN_VERSION_aeson(1,5,0)
import Data.Coerce
#endif

-- | Various reason for failed parsing.
--
-- @since 0.1.0
data ParserError
  = AttoParserError ParseError
  -- ^ Attoparsec parser failure
  | AesonParserError String
  -- ^ Aeson parser failure
  | NotTerminatedInput
  -- ^ Parser failure when end of input was reached without proper termination.
  deriving Show
instance Exception ParserError


-- | Parse a top level object into a stream of key/value pairs. Throws a
-- `ParserError` on invalid input, see `conduitObjectEither` for more graceful
-- error handling.
--
-- ====__Examples__
--
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeApplications
-- >>> import Conduit
-- >>> let input = "{ \"foo\": 1, \"bar\": 2, \"baz\": 3 }"
-- >>> runConduit $ yield input .| conduitObject @String @Int .| printC
-- ("foo",1)
-- ("bar",2)
-- ("baz",3)
--
-- @since 0.1.0
conduitObject ::
     forall k v m. (FromJSONKey k, FromJSON v, MonadThrow m)
  => ConduitM BS.ByteString (k, v) m ()
conduitObject = conduitObjectEither .| mapMC (either throwM pure)

-- | Same as `conduitObject`, except fails gracefully. Parse a top level object
-- into a stream of key/value pairs with potential failures as @`Left` `ParserError`@.
--
-- @since 0.1.0
conduitObjectEither ::
     forall k v m. (FromJSONKey k, FromJSON v, Monad m)
  => ConduitM BS.ByteString (Either ParserError (k, v)) m ()
conduitObjectEither = conduitObjectParserEither .| stopOnNothing .| mapC toKeyValue
  where
    _id x = x -- work around an aeson rewrite rule.
    toKeyValue (Left err) = Left err
    toKeyValue (Right (_, (k, v))) =
      first AesonParserError $ do
        key <-
          case fromJSONKey of
#if MIN_VERSION_aeson(1,5,0)
            FromJSONKeyCoerce       -> Right $ coerce k
#else
            FromJSONKeyCoerce {}
               | FromJSONKeyText f <- fmap _id fromJSONKey -> Right $ f k
               | otherwise -> error "Impossible: failed to convert coercible FromJSONKeyCoerce"
#endif
            FromJSONKeyText f       -> Right $ f k
            FromJSONKeyTextParser p -> Aeson.parseEither p k
            FromJSONKeyValue p      -> Aeson.parseEither p (String k)
        val <- Aeson.parseEither Aeson.parseJSON v
        Right (key, val)

-- | Parse a top level key value mapping. Expects opening and closing braces
-- @'{'@ and @'}'@. `Nothing` indicates terminating closing curly brace has been
-- reached.
--
-- @since 0.1.0
conduitObjectParserEither ::
     Monad m
  => ConduitM  BS.ByteString (Either ParseError (PositionRange, Maybe (T.Text, Value))) m ()
conduitObjectParserEither = do
  sinkParserEither objectEntryPrefixParser >>= \case
    Left err -> yield $ Left err
    Right () -> conduitParserEither (objectEntryMaybeParser commaParser)

-- | Expects that there is no opening of a top level object curly brace @{@, but
-- will stop as soon as the closing one @}@ is reached. It is suitable for infinite
-- streams of key value pairs delimited by a custom character (eg. a new line)
--
-- >>> import Conduit
-- >>> import Data.ByteString.Char8 (ByteString, pack)
-- >>> import Data.Attoparsec.ByteString.Char8 (char8)
-- >>> let input = pack "\"foo\":1|\"bar\":2|" :: ByteString
-- >>> let parser = conduitObjectParserNoStartEither (char8 '|')
-- >>> runConduit (yield input .| parser .| printC)
-- Right (1:1 (0)-1:9 (8),("foo",Number 1.0))
-- Right (1:9 (8)-1:17 (16),("bar",Number 2.0))
--
-- @since 0.1.0
conduitObjectParserNoStartEither ::
     forall m a. Monad m
  => Atto.Parser a
  -- ^ Delimiter parser (in JSON it is a comma @','@)
  -> ConduitM BS.ByteString (Either ParseError (PositionRange, (T.Text, Value))) m ()
conduitObjectParserNoStartEither = conduitParserEither . objectEntryParser


-- | Parse a top level array into a stream of json values.  Throws a
-- `ParserError` on invalid input, see `conduitArrayEither` for more graceful
-- error handling.
--
-- ====__Examples__
--
-- >>> :set -XTypeApplications
-- >>> :set -XOverloadedStrings
-- >>> import Conduit
-- >>> runConduit $ yield ("[1,2,3,4]") .| conduitArray @Int .| printC
-- 1
-- 2
-- 3
-- 4
--
-- @since 0.1.0
conduitArray ::
     forall v m. (FromJSON v, MonadThrow m)
  => ConduitM BS.ByteString v m ()
conduitArray = conduitArrayEither .| mapMC (either throwM pure)

-- | Parse a top level key value mapping. Expects opening and closing braces
-- @'{'@ and @'}'@. `Nothing` indicates terminating closing curly brace has been
-- reached.
--
-- @since 0.1.0
conduitArrayEither ::
     forall v m. (FromJSON v, Monad m)
  => ConduitM BS.ByteString (Either ParserError v) m ()
conduitArrayEither = conduitArrayParserEither .| stopOnNothing .| mapC toValue
  where
    toValue (Left err) = Left err
    toValue (Right (_, v)) = first AesonParserError $ Aeson.parseEither Aeson.parseJSON v

-- | Parse a top level array as a stream of JSON values. Expects opening and
-- closing braket @'['@ and @']'@. `Nothing` indicates terminating closing square
-- braket has been reached.
--
-- @since 0.1.0
conduitArrayParserEither ::
     Monad m
  => ConduitM  BS.ByteString (Either ParseError (PositionRange, Maybe Value)) m ()
conduitArrayParserEither = do
  sinkParserEither valuePrefixParser >>= \case
    Left err -> yield $ Left err
    Right () -> conduitParserEither (valueMaybeParser commaParser)

-- | Parse a stream of JSON values. Expects that there are no opening or closing
-- top level array braces @[@ and @]@. Could be very useful for consuming
-- infinite streams of log entries, where each entry is formatted as a JSON
-- value.
--
-- ====__Examples__
--
-- Parse a new line delimited JSON values.
--
-- >>> import Conduit
-- >>> import Data.ByteString.Char8 (ByteString, pack)
-- >>> import Data.Attoparsec.ByteString.Char8 (char8)
-- >>> let input = pack "{\"foo\":1}\n{\"bar\":2}\n" :: ByteString
-- >>> let parser = conduitArrayParserNoStartEither (char8 '\n')
-- >>> runConduit (yield input .| parser .| printC)
-- Right (1:1 (0)-2:1 (10),Object (fromList [("foo",Number 1.0)]))
-- Right (2:1 (10)-3:1 (20),Object (fromList [("bar",Number 2.0)]))
--
-- Or a simple comma delimited list:
--
-- >>> runConduit $ yield (pack "1,2,3,\"Haskell\",") .| conduitArrayParserNoStartEither (char8 ',') .| printC
-- Right (1:1 (0)-1:3 (2),Number 1.0)
-- Right (1:3 (2)-1:5 (4),Number 2.0)
-- Right (1:5 (4)-1:7 (6),Number 3.0)
-- Right (1:7 (6)-1:17 (16),String "Haskell")
--
-- @since 0.1.0
conduitArrayParserNoStartEither ::
     forall m a. Monad m
  => Atto.Parser a
  -- ^ Delimiter parser (in JSON it is a comma @','@)
  -> ConduitM BS.ByteString (Either ParseError (PositionRange, Value)) m ()
conduitArrayParserNoStartEither = conduitParserEither . valueParser


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

-- | Skips all spaces and newlines
--
-- @since 0.1.0
skipSpace :: Atto.Parser ()
skipSpace = Atto.skipWhile $ \w -> w == 0x20 || w == 0x0a || w == 0x0d || w == 0x09

-- | Use a comma for delimiter.
--
-- @since 0.1.0
commaParser ::
     Char
  -- ^ Terminating character.
  -> Atto.Parser ()
commaParser = delimiterParser (Atto.word8 0x2c Atto8.<?> "','")

-- | Parser for delimiter with terminating character
--
-- @since 0.1.0
delimiterParser :: Atto.Parser a -> Char -> Atto.Parser ()
delimiterParser dp t =
  skipSpace <* (void dp <|> expectTermination)
  where
    expectTermination =
      Atto8.peekChar >>= \case
        Just c
          | c /= t -> fail $ "Unexpected delimiter: " ++ show c
        _ -> pure ()

-- | Consume @'['@ with all preceeding space characters
--
-- @since 0.1.0
valuePrefixParser :: Atto.Parser ()
valuePrefixParser = skipSpace <* Atto8.char '['

-- | Parse a JSON value potentially prefixed by whitespace followed by a suffix
--
-- @since 0.1.0
valueParser ::
     Atto.Parser a
  -- ^ Suffix parser
  -> Atto.Parser Aeson.Value
valueParser dp = skipSpace *> json' <* dp

-- | Parse a JSON value followed either by a delimiter or terminating
-- character @']'@, which is also supplied to the delimiter parser. Nothing is
-- returned when terminating character is reached.
--
-- @since 0.1.0
valueMaybeParser ::
     (Char -> Atto.Parser a)
  -- ^ Delimiter parser (accepts terminating character as argument)
  -> Atto.Parser (Maybe Aeson.Value)
valueMaybeParser dp =
  let t = ']'
   in skipSpace *> ((Nothing <$ Atto8.char t) <|> (Just <$> json' <* dp t))

-- | Consume @'{'@ with all preceeding space characters
--
-- @since 0.1.0
objectEntryPrefixParser :: Atto.Parser ()
objectEntryPrefixParser = skipSpace <* Atto8.char '{'


-- | Parse JSON object key followed by a colon
--
-- @since 0.1.0
keyParser :: Atto.Parser T.Text
keyParser =
  skipSpace *>
  (Aeson.jstring Atto.<?> "key") <*
  skipSpace <*
  (Atto.word8 0x3a Atto.<?> "':'")

-- | Parse a JSON key value pair followed by a suffix
--
-- @since 0.1.0
objectEntryParser ::
     Atto.Parser a
  -- ^ Suffix parser
  -> Atto.Parser (T.Text, Aeson.Value)
objectEntryParser dp = (,) <$> keyParser <*> valueParser dp


-- | Parse JSON key value pairs followed either by a delimiter or terminating
-- character @']'@, which is also supplied to the delimiter parser. Nothing is
-- returned when terminating character is reached.
--
-- @since 0.1.0
objectEntryMaybeParser :: (Char -> Atto.Parser a) -> Atto.Parser (Maybe (T.Text, Aeson.Value))
objectEntryMaybeParser dp =
  let t = '}'
   in skipSpace *>
      ((Nothing <$ Atto8.char t) <|> (Just <$> objectEntryParser (dp t)))
