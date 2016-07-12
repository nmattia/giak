{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async                      (async, cancel, wait)
import Control.Concurrent.STM.TMQueue                (TMQueue, newTMQueueIO,
                                                      tryReadTMQueue,
                                                      writeTMQueue)
import Control.Monad
import Control.Monad.Extra
import Control.Monad.STM                             (atomically)
import Data.List.NonEmpty                            (NonEmpty (..))
import Data.Maybe                                    (isJust)
import Data.Monoid                                   ((<>))
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.PackageDescription.Parse         (readPackageDescription)
import IOCTL                                         (writeOnTTY)
import System.Directory                              (doesFileExist,
                                                      findExecutable)
import System.FilePath.Glob                          (namesMatching)

import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.Verbosity          as Verbosity
import qualified Wybor

data GiakResult = GiakResult BuildTool Cabal.Executable

data BuildTool = Cabal | Stack

type GatherChan = TMQueue (NonEmpty (T.Text, T.Text))

buildCommand :: GiakResult -> T.Text
buildCommand = T.pack . unwords . buildCommand'
  where
    buildCommand' (GiakResult Stack exe) = ["stack", "exec", Cabal.exeName exe]
    buildCommand' (GiakResult Cabal exe) = ["cabal", "exec", Cabal.exeName exe]

main :: IO ()
main = do
    results <- newTMQueueIO
    gatherer <- async $ gatherExecutables results
    async (displayChoices results) >>= wait
    cancel gatherer

gatherExecutables :: GatherChan -> IO ()
gatherExecutables chan =
    loadResults >>= mapM_ (\res -> do
        let res' = (toDisplayedText res, buildCommand res)
        atomically $ writeTMQueue chan $  res' :| [])

toDisplayedText :: GiakResult -> T.Text
toDisplayedText res@(GiakResult _ exe) = name <> "   > " <> command
  where command = buildCommand res
        name = T.pack $ Cabal.exeName exe

displayChoices :: GatherChan -> IO ()
displayChoices results = do
    c <- Wybor.select . Wybor.fromIO . atomically $ tryReadTMQueue results
    case c of
        Right (Just c') -> writeOnTTY $ T.encodeUtf8 c'
        _ -> return ()

loadResults :: IO [GiakResult]
loadResults = do
    ss <- checkCabal
    ifM isStackReady
        (return $ toStackExe <$> ss)
        $ return ss
  where
    toStackExe (GiakResult _ exe) = GiakResult Stack exe

isStackReady :: IO Bool
isStackReady = isStackInstalled &&^ hasStackYaml
  where
    isStackInstalled = isJust <$> findExecutable "stack"
    hasStackYaml = doesFileExist "stack.yaml"

checkCabal :: IO [GiakResult]
checkCabal = namesMatching "*.cabal" >>= fromCabalFiles

fromCabalFiles :: [FilePath] -> IO [GiakResult]
fromCabalFiles fs = do
    ress <- forM fs $ \f -> (mkResult <$>) <$> readExecutables f
    return $ concat ress

readExecutables :: FilePath -> IO [Cabal.Executable]
readExecutables f =
  Cabal.executables . flattenPackageDescription <$>
    readPackageDescription Verbosity.silent f

mkResult :: Cabal.Executable -> GiakResult
mkResult = GiakResult Cabal
