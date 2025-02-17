{-# LANGUAGE OverloadedStrings #-}

module Main where

import Blaze.ByteString.Builder (copyByteString, fromByteString)
import Control.Concurrent
import Data.ByteString.UTF8 qualified as BU
import Data.Time qualified as Time
import Data.Time.Format.ISO8601 qualified as Time
import Network.HTTP.Types (status200)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp
import System.Exit
import System.IO qualified as IO
import System.Process qualified as Proc

data PonyResult = PonyResult ExitCode BU.ByteString

data CountedChannel a = CountedChannel
    { amnt :: MVar Int
    , chan :: Chan a
    }

ponyBufferSize :: Int
ponyBufferSize = 100

port :: Int
port = 3000

logS :: String -> IO ()
logS s = do
    currentTime <- Time.getCurrentTime
    IO.hPutStrLn IO.stderr (Time.iso8601Show currentTime <> " -- " <> s)

main :: IO ()
main = do
    logS $ "Listening on port " <> show port
    ponyBuffer <- do
        myAmnt <- newMVar 0
        myChan <- newChan
        pure $ CountedChannel myAmnt myChan
    _threadID <- forkIO (generatorThread ponyBuffer)
    run port (app ponyBuffer)

app :: CountedChannel PonyResult -> Wai.Request -> (Wai.Response -> IO a) -> IO a
app ponyBuffer req respond = do
    logS $ "Request From " <> (show . Wai.remoteHost $ req)
    pollForMore ponyBuffer
    ponyResult <- modifyMVar (amnt ponyBuffer) $ \a -> do
        ponyResult <- readChan (chan ponyBuffer)
        -- In theory, after the subtraction, a could be
        -- negative. This is okay.
        pure (a - 1, ponyResult)
    case ponyResult of
        PonyResult ExitSuccess ponyData -> do
            respond $ successResponse ponyData
        PonyResult failureExitCode _ -> do
            respond $ errorResponse failureExitCode

pollForMore :: CountedChannel a -> IO ()
pollForMore channel =
    readMVar (amnt channel)
        >>= \count ->
            if count > 0
                then do
                    pure ()
                else do
                    logS "Ran out of ponies... waiting"
                    threadDelay 300_000
                    pollForMore channel

generatorThread :: CountedChannel PonyResult -> IO ()
generatorThread ponyBuffer = do
    count <- readMVar (amnt ponyBuffer)
    if count < ponyBufferSize
        then do
            logS ("Generating pony...")
            newPony <- generatePony
            handleGenResult newPony
        else do
            threadDelay 1_000_000
            generatorThread ponyBuffer
  where
    handleGenResult (Just newPony) = do
        logS ("Success.")
        -- We specifically don't lock the amnt along
        -- with the channel to prevent deadlock.
        writeChan (chan ponyBuffer) newPony
        newPonyCount <- modifyMVar (amnt ponyBuffer) $ \a -> do
            pure $ (a + 1, a + 1)
        logS ("New pony added: now at " <> show newPonyCount)
        generatorThread ponyBuffer
    handleGenResult Nothing = do
        logS ("Timed out.")
        generatorThread ponyBuffer

generatePony :: IO (Maybe PonyResult)
generatePony = do
    fortuneOutput <- Proc.readProcess "fortune" [] ""
    ponysayOutput <- Proc.readProcess "ponysay" ["-X", "-W", "100"] fortuneOutput
    htmlOutput <- Proc.readProcess "ansi2html" [] (ponysayOutput <> "\n\n  Refresh for a new pony fortune.")
    pure $ Just (PonyResult ExitSuccess (BU.fromString htmlOutput))

errorResponse :: ExitCode -> Wai.Response
errorResponse exitCode =
    Wai.responseBuilder status200 [("Content-Type", "text/plain")]
        . mconcat
        . map copyByteString
        $ ["Error: " <> (BU.fromString (show exitCode))]

successResponse :: BU.ByteString -> Wai.Response
successResponse output =
    Wai.responseBuilder status200 [("Content-Type", "text/html")] $ fromByteString output
