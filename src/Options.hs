-- | opt-parse applicative description of command line options
module Options (Options(..), parseOptions)
    where

import Relude

import qualified Options.Applicative as Opt
import Path (Dir, File, SomeBase, parseSomeDir, parseSomeFile)



-- | The program options
data Options = Options
  { optInputFile :: SomeBase File -- ^ The file to parse
  , optOutputDir :: SomeBase Dir  -- ^ The output directory
  }


-- | The description of the command line options
options :: Opt.Parser Options
options = Options
      <$> Opt.option (Opt.maybeReader parseSomeFile)
          ( Opt.long "input"
         <> Opt.metavar "INPUT"
         <> Opt.help "Input file" )
      <*> Opt.option (Opt.maybeReader parseSomeDir)
          ( Opt.long "output"
         <> Opt.metavar "OUTPUT"
         <> Opt.help "Output directory" )


-- | Run the command line parser with the command interpreter
parseOptions :: IO Options
parseOptions =  Opt.execParser opts
  where opts = Opt.info (options <**> Opt.helper) Opt.idm

