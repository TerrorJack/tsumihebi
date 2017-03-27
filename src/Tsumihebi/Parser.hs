{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}

module Tsumihebi.Parser
    ( identifier
    ) where

import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Prim
import Tsumihebi.Syntax

identifier
    :: (MonadParsec e s m, Token s ~ Char)
    => m Identifier
identifier = do
    c <- satisfy $ \x -> isAscii x && (isLetter x || x == '_')
    cs <- many $ satisfy $ \x -> isAscii x && (isAlphaNum x || x == '_')
    pure $ Identifier $ c : cs
