-- |
-- Module      :  Portager.Options
-- Copyright   :  (C) 2017 Jiri Marsicek
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Jiri Marsicek <jiri.marsicek@gmail.com>
--
-- This module provides functionality for parsing command line options and executing arbitrary 'IO' program 
-- with 'Options' as an input.
--
module Portager.Options where

import Options.Applicative

import Data.Text (Text)
import System.FilePath (FilePath)

type WorldSet = Text

data Options = Options
  { _targetDir :: FilePath
  , _worldSets :: FilePath
  } deriving (Eq, Show)


options :: Parser Options
options = Options 
  <$> strOption
      ( long "target"
      <> short 't'
      <> metavar "TARGET"
      <> value "/etc/portage"
      <> showDefault
      <> help "Target directory"
      )
  <*> strOption
      ( long "worldsets" 
      <> short 'w'
      <> metavar "WORLD_SETS"
      <> value "/var/lib/portage/world_sets"
      <> showDefault
      <> help "Portage World Sets File"
      )

withOptions :: (Options -> IO a) -> IO a
withOptions a = execParser opts >>= a
 where
    opts = info (helper <*> options)
      ( fullDesc
      <> progDesc "Generate portage configuration files from single file"
      <> header "portager - portage configuration" 
      )
