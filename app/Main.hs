{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Blaze.ByteString.Builder (copyByteString, fromByteString)
import Control.Concurrent (
    Chan,
    MVar,
    forkIO,
    modifyMVar,
    newChan,
    newMVar,
    readChan,
    readMVar,
    threadDelay,
    writeChan,
 )
import Control.Concurrent.Async as Async
import Data.ByteString.UTF8 qualified as BU
import Data.Time qualified as Time
import Data.Time.Format.ISO8601 qualified as Time
import Network.HTTP.Types (status200)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Options.Applicative qualified as Args
import System.Exit (ExitCode (..))
import System.IO qualified as IO
import System.Process qualified as Proc

-- ------------------------------------------------------------------------------------------------
-- Types
-- ------------------------------------------------------------------------------------------------

data Arguments = Arguments
    { argsPort :: Int
    , quiet :: Bool
    , argsBufferSize :: Int
    }

data PonyResult = PonyResult ExitCode BU.ByteString

data CountedChannel a = CountedChannel
    { ccAmount :: MVar Int
    , chan :: Chan a
    , ccMaxSize :: Int
    }

type Logger = (String -> IO ())

-- ------------------------------------------------------------------------------------------------
-- Functions
-- ------------------------------------------------------------------------------------------------

main :: IO ()
main = do
    args <- Args.execParser prgrmOpts
    let logS = if quiet args then noLogs else stdoutLogs
    logS ("Listening on port " <> show (argsPort args))
    ponyBuffer <- do
        amnt <- newMVar 0
        myChan <- newChan
        pure $ CountedChannel amnt myChan (argsBufferSize args)
    let generatorExpr = (generatorThread logS ponyBuffer)
    let warpExpr = Warp.run (argsPort args) (app logS ponyBuffer)
    Async.race_ generatorExpr warpExpr
  where
    prgrmOpts =
        Args.info
            (parseArgs Args.<**> Args.helper)
            (Args.header "ponysay-fortune-webserver - HTTP server for random pony quoting.")

parseArgs :: Args.Parser Arguments
parseArgs = do
    quiet <-
        Args.switch
            ( Args.long "quiet"
                <> Args.short 'q'
                <> Args.help "Don't produce any logging."
            )
    argsPort <-
        Args.option
            Args.auto
            ( Args.long "port"
                <> Args.help "Port to listen on."
                <> Args.showDefault
                <> Args.value defaultPort
                <> Args.metavar "PORTNUM"
            )
    argsBufferSize <-
        Args.option
            Args.auto
            ( Args.long "buffer-size"
                <> Args.short 'b'
                <> Args.help "Number of ponies to pre-buffer."
                <> Args.value defaultPonyBufferSize
                <> Args.metavar "NUM")
    pure Arguments{..}

-- Web server -------------------------------------------------------------------------------------

app :: Logger -> CountedChannel PonyResult -> Wai.Request -> (Wai.Response -> IO a) -> IO a
app logS ponyBuffer req respond = do
    logS $ "Request From " <> (show . Wai.remoteHost $ req)
    pollForMore logS ponyBuffer
    ponyResult <- modifyMVar (ccAmount ponyBuffer) $ \a -> do
        ponyResult <- readChan (chan ponyBuffer)
        -- In theory, after the subtraction, a could be
        -- negative. This is okay.
        pure (a - 1, ponyResult)
    case ponyResult of
        PonyResult ExitSuccess ponyData -> do
            respond $ successResponse ponyData
        PonyResult failureExitCode _ -> do
            respond $ errorResponse failureExitCode

errorResponse :: ExitCode -> Wai.Response
errorResponse exitCode =
    Wai.responseBuilder status200 [("Content-Type", "text/plain")]
        . mconcat
        . map copyByteString
        $ ["Error: " <> (BU.fromString (show exitCode))]

successResponse :: BU.ByteString -> Wai.Response
successResponse output =
    Wai.responseBuilder status200 [("Content-Type", "text/html")] $ fromByteString output

pollForMore :: Logger -> CountedChannel a -> IO ()
pollForMore logS channel =
    readMVar (ccAmount channel)
        >>= \count ->
            if count > 0
                then pure ()
                else do
                    logS "Ran out of ponies... waiting"
                    threadDelay 300_000
                    pollForMore logS channel

-- Generator --------------------------------------------------------------------------------------

generatorThread :: Logger -> CountedChannel PonyResult -> IO ()
generatorThread logS ponyBuffer = do
    count <- readMVar (ccAmount ponyBuffer)
    if count < ccMaxSize ponyBuffer
        then do
            logS "Generating pony..."
            newPony <- generatePony
            handleGenResult newPony
        else do
            threadDelay 1_000_000
            generatorThread logS ponyBuffer
  where
    handleGenResult (Just newPony) = do
        logS "Success."
        -- We specifically don't lock the ccAmount along
        -- with the channel to prevent deadlock.
        writeChan (chan ponyBuffer) newPony
        newPonyCount <- modifyMVar (ccAmount ponyBuffer) $ \a -> do
            pure $ (a + 1, a + 1)
        logS ("New pony added: now at " <> show newPonyCount)
        generatorThread logS ponyBuffer
    handleGenResult Nothing = do
        logS "Timed out."
        generatorThread logS ponyBuffer

generatePony :: IO (Maybe PonyResult)
generatePony = do
    fortuneOutput <- Proc.readProcess "fortune" [] ""
    ponysayOutput <- Proc.readProcess "ponysay" ["-X", "-W", "100"] fortuneOutput
    htmlOutput <-
        Proc.readProcess
            "ansi2html"
            ["-a"]
            (ponysayOutput
                <> "\n\n  refresh for a new pony fortune."
                <> "\n  or you can go back to https://crystalwobsite.gay")
    pure $ Just (PonyResult ExitSuccess (BU.fromString htmlOutput))

-- Utils ------------------------------------------------------------------------------------------

stdoutLogs :: String -> IO ()
stdoutLogs s = do
    currentTime <- Time.getCurrentTime
    IO.hPutStrLn IO.stdout (Time.iso8601Show currentTime <> " -- " <> s)

noLogs :: String -> IO ()
noLogs _ = pure ()

defaultPonyBufferSize :: Int
defaultPonyBufferSize = 30

defaultPort :: Int
defaultPort = 3000
