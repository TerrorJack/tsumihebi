module Tsumihebi.Syntax
    ( Identifier(..)
    , IntegerConstant(..)
    , CharacterConstant(..)
    , StringLiteral(..)
    ) where

newtype Identifier = Identifier
    { getIdentifier :: String
    }

newtype IntegerConstant = IntegerConstant
    { getIntegerConstant :: Integer
    }

newtype CharacterConstant = CharacterConstant
    { getCharacterConstant :: Char
    }

newtype StringLiteral = StringLiteral
    { getStringLiteral :: String
    }
