{-# LANGUAGE OverloadedStrings, ViewPatterns, TemplateHaskell #-}

module Main where

import Network.DBus.Connection
import Network.DBus.Message
import Network.DBus.Value

import Data.Maybe

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Identity

import System.Process
import System.Exit
import System.Environment
import System.Console.GetOpt
import System.Directory
import System.FilePath

import Data.String

import System.Log.Logger.TH
import qualified System.Log.Logger as HSL

deriveNamedLoggers "mute-unmute" "HSL" [HSL.DEBUG, HSL.INFO, HSL.ERROR, HSL.WARNING]
-- deriveLoggers "HSL" [HSL.DEBUG, HSL.INFO, HSL.ERROR, HSL.WARNING]

instance IsString DString where
    fromString str = runIdentity (mkDString str)

data RunMode = StoreMute   --store-mute
             | StoreUnmute --store-unmute
             | StoreAll    --store
             | Daemon      --daemon, -d
             | Help        --help,-h,-?
             deriving (Show)

data Config = Config { mute :: FilePath
                     , unmute :: FilePath
                     , appPath :: FilePath
                     }
            deriving (Show)

type ConfigM a = ReaderT Config IO a

-- Example (unlock) message. Lock would set True instead of False.

-- Message {
--  mType = Signal,
--  mFlags = [NoReplyExpected],
--  mSerial = 134,
--  mPath = Just "/org/gnome/ScreenSaver",
--  mInterface = Just "org.gnome.ScreenSaver",
--  mMember = Just "ActiveChanged",
--  mErrorName = Nothing,
--  mReplySerial = Nothing,
--  mDestination = Nothing,
--  mSender = Just ":1.19",
--  mBody = [Variant (False) {- b -}]
--         }

-- | constants

muteConfig :: String
muteConfig = "mute.ctl"
unmuteConfig :: String
unmuteConfig = "unmute.ctl"
alsaCmd :: String
alsaCmd = "alsactl"
programName :: String
programName = "mute-unmute"

-- | helpers

checkExitCode :: (Monad m) => String -> ExitCode -> m ()
checkExitCode _ ExitSuccess = return ()
checkExitCode cmd ef = fail (show (cmd,ef))


alsactl :: (MonadIO m) => String -> String -> m ()
alsactl cmd file = liftIO (checkExitCode "alsactl" =<< rawSystem alsaCmd [cmd,"-f",file])

mkConfig :: FilePath -> Config
mkConfig aPath = Config { mute = aPath </> muteConfig
                        , unmute = aPath </> unmuteConfig
                        , appPath = aPath
                        }

-- | message handling

handleMessage :: Message -> ConfigM ()
handleMessage msg@(mBody -> [var]) = debugM (show msg) >>
                                     case fromVariant var of
                                     Nothing -> errorM ("Something went wrong, bad type:" ++ show msg)
                                     Just True -> handleLock
                                     Just False -> handleUnlock

handleMessage msg = errorM ("Wrong number of elements in mBody: " ++ show msg)

handleLock, handleUnlock :: ConfigM ()
handleLock = alsactl "restore" . mute =<< ask
handleUnlock = alsactl "restore" . unmute =<< ask

storeMute, storeUnmute :: ConfigM ()
storeUnmute = alsactl "store" . unmute =<< ask
storeMute = alsactl "store" . mute =<< ask

-- | 'main' functions

askMute, askUnmute :: ConfigM ()
askMute = returnMsg "Mute" >> storeMute
askUnmute = returnMsg "Unmute" >> storeUnmute

returnMsg :: (MonadIO m) => String -> m ()
returnMsg msg = liftIO $ (putStrLn (msg ++ " sound card and press RETURN") >> getLine >> return ())

checkAppDir :: ConfigM ()
checkAppDir = do
  app <- appPath <$> ask
  appExist <- liftIO $ doesDirectoryExist app
  when (not appExist) . liftIO $ do
                infoM "Creating config directory..."
                createDirectory app

checkConfig :: ConfigM ()
checkConfig = do
  checkAppDir

  mu  <- mute <$> ask
  muExist <- liftIO $ doesFileExist mu
  when (not muExist) $ do
                infoM "Mute config is missing..."
                askMute

  un  <- unmute <$> ask
  unExist <- liftIO $ doesFileExist un
  when (not unExist) $ do
                infoM "Unmute config is missing..."
                askUnmute

daemon :: ConfigM ()
daemon = do
  checkConfig
  path <- mkObjectPath "/org/gnome/ScreenSaver"
  let clauses = [ MatchType Signal
                , MatchInterface "org.gnome.ScreenSaver"
                , MatchMember "ActiveChanged"
                , MatchPath path]

  conf <- ask
  liftIO $ do
            session <- fromJust <$> getSessionBusAddress
            conn <- connectToBus session
            addHandler conn (Just $ clauses) (\m -> runReaderT (handleMessage m) conf)
            forever (threadDelay (10^6))

programOpt :: [OptDescr RunMode]
programOpt = [ Option [] ["store"] (NoArg StoreAll) "ask for mute and unmute configurations"
             , Option [] ["store-mute"] (NoArg StoreMute) "ask for mute configuration"
             , Option [] ["store-unmute"] (NoArg StoreUnmute) "ask for unmute configuration"
             , Option ['d'] ["daemon"] (NoArg Daemon) "wait for screensaver state changes"
             , Option ['h','?'] ["help"] (NoArg Help) "show help"
             ]

parseOptions :: [String] -> IO RunMode
parseOptions args = do
  case getOpt Permute programOpt args of
    ([],[],[]) -> return Daemon
    ([o],[],[]) -> return o
    other -> fail $ show ("Bad program options" :: String,other)

main :: IO ()
main = do
  HSL.updateGlobalLogger programName (HSL.setLevel HSL.DEBUG)
  applicationPath <- getAppUserDataDirectory programName
  m <- parseOptions =<< getArgs

  (flip runReaderT) (mkConfig applicationPath) $
   case m of
    StoreMute -> checkAppDir >> askMute
    StoreUnmute -> checkAppDir >> askUnmute
    StoreAll -> checkAppDir >> askMute >> askUnmute
    Daemon -> daemon
    Help -> liftIO $ putStrLn (usageInfo programName programOpt)
