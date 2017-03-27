{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}

module Tsumihebi.Parser
    ( identifier
    , integerConstant
    ) where

import Data.Char
import Data.Functor
import Text.Megaparsec
import qualified Text.Megaparsec.Lexer as Lex
import Text.Megaparsec.Prim
import Tsumihebi.Syntax

lexeme
    :: (MonadParsec e s m, Token s ~ Char)
    => String -> m a -> m a
lexeme n p = label n $ space *> p

identifier
    :: (MonadParsec e s m, Token s ~ Char)
    => m Identifier
identifier =
    lexeme "identifier" $ do
        c <- satisfy $ \x -> isAscii x && (isLetter x || x == '_')
        cs <- many $ satisfy $ \x -> isAscii x && (isAlphaNum x || x == '_')
        pure $ Identifier $ c : cs

integerConstant
    :: (MonadParsec e s m, Token s ~ Char)
    => m IntegerConstant
integerConstant =
    lexeme "integerConstant" $ IntegerConstant <$> choice [hex, oct, dec]
  where
    hex =
        try $ do
            void $ char '0'
            void $ char 'x' <|> char 'X'
            Lex.hexadecimal
    oct =
        try $ do
            void $ lookAhead $ char '0'
            Lex.octal
    dec = try Lex.integer
