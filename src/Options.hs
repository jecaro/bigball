-- | opt-parse applicative description of command line options
module Options (Options(..), runCliParser)
    where

import Relude

import qualified Options.Applicative as Opt


-- | The program options
data Options = Options
  { optInputFile :: String  -- ^ The file to parse
  , optOutputDir :: String -- ^ The output directory
  }


-- | The description of the command line options
options :: Opt.Parser Options
options = Options
      <$> Opt.strOption
          ( Opt.long "input"
         <> Opt.metavar "INPUT"
         <> Opt.help "Input file" )
      <*> Opt.strOption
          ( Opt.long "output"
         <> Opt.metavar "OUTPUT"
         <> Opt.help "Output directory" )


-- | Run the command line parser with the command interpreter
runCliParser :: (Options -> IO a) -> IO a
runCliParser action =  Opt.execParser opts >>= action
  where opts = Opt.info (options <**> Opt.helper) Opt.idm

