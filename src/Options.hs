module Options (Options(..), runCliParser)
    where

import Relude

import qualified Options.Applicative as Opt


data Options = Options
  { optInputFile :: String
  , optOutputFile :: String
  }


options :: Opt.Parser Options
options = Options
      <$> Opt.strOption
          ( Opt.long "input"
         <> Opt.metavar "INPUT"
         <> Opt.help "Input file" )
      <*> Opt.strOption
          ( Opt.long "output"
         <> Opt.metavar "OUTPUT"
         <> Opt.help "Output file" )


runCliParser :: (Options -> IO a) -> IO a
runCliParser action =  Opt.execParser opts >>= action
  where opts = Opt.info (options <**> Opt.helper) Opt.idm

