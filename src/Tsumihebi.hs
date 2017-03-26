{-# LANGUAGE OverloadedStrings #-}

module Tsumihebi where

import Control.Concurrent.Async
import Control.DeepSeq
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as LBS
import qualified Data.ByteString.Char8 as BS (lines)
import qualified Data.ByteString.Lazy.UTF8 as LBS
import Language.Preprocessor.Cpphs
import System.Directory.Extra
import System.FilePath
import System.IO
import System.IO.Temp

generateFakeHeader :: CpphsOptions -> FilePath -> IO String
generateFakeHeader opts bp = do
    ps <- listFilesRecursive bp
    bs <-
        forConcurrently [p | p <- ps, takeExtension p == ".h"] $ \p -> do
            f <- BS.readFile p
            pure $
                mconcat
                    [ LBS.byteString l `mappend` LBS.char7 '\n'
                    | l <- BS.lines f
                    , not $ BS.isPrefixOf "#include" l
                    ]
    withSystemTempFile "" $ \tmp h -> do
        hClose h
        res <-
            runCpphs opts tmp $ LBS.toString $ LBS.toLazyByteString $ mconcat bs
        res `deepseq` pure res

defOpts :: CpphsOptions
defOpts =
    defaultCpphsOptions
    { boolopts =
          defaultBoolOptions
          {locations = False, lang = False, stripEol = True, stripC89 = True}
    }
