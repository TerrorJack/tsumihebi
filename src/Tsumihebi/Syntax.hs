module Tsumihebi.Syntax
    ( Identifier(..)
    , IntegerConstant(..)
    , FloatingConstant(..)
    , CharacterConstant(..)
    , StringLiteral(..)
    ) where

newtype Identifier = Identifier
    { getIdentifier :: String
    }

newtype IntegerConstant = IntegerConstant
    { getIntegerConstant :: Int
    }

newtype FloatingConstant = FloatingConstant
    { getFloatingConstant :: Double
    }

newtype CharacterConstant = CharacterConstant
    { getCharacterConstant :: Char
    }

newtype StringLiteral = StringLiteral
    { getStringLiteral :: String
    }
