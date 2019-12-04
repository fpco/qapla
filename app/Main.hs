{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Qapla
import RIO
import Options.Applicative.Simple
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Autohead
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.RequestLogger
import qualified Paths_qapla

data Options = Options
  { dir :: !FilePath
  , mhost :: !(Maybe String)
  , port :: !Int
  }

main :: IO ()
main = do
  (Options {..}, ()) <- simpleOptions
    $(simpleVersion Paths_qapla.version)
    "Host statically generated site"
    "Host statically generated site"
    (Options
       <$> strOption
                  ( long "dir"
                 <> help "Directory to host"
                 <> metavar "DIRECTORY"
                  )
       <*> optional (strOption
                  ( long "host"
                 <> help "Host to listen on, either IP address, *4, *6, or *"
                 <> metavar "HOST"
                  ))
       <*> option auto
                  ( long "port"
                 <> help "Port to listen on"
                 <> metavar "PORT"
                  )
    )
    empty
  app <- qapla dir
  let settings = setPort port $ maybe id (setHost . fromString) mhost defaultSettings
  runSettings settings $
    autohead $
    gzip def $
    logStdout app
