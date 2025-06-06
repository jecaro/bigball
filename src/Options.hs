-- | opt-parse applicative description of command line options
module Options (Options (..), Error, parse, render) where

import qualified Options.Applicative as Opt
import Path (Dir, File, SomeBase, parseSomeDir, parseSomeFile)
import Relude

-- | The program options
data Options = Options
  { -- | The file to parse
    optInputFile :: SomeBase File,
    -- | The output directory
    optOutputDir :: SomeBase Dir
  }

-- | The different kinds of error
data Error = Parse Text | CompletionInvoked

-- | Run the command line parser with the command interpreter
parse :: String -> [String] -> Either Error Options
parse progName args =
  let opts = Opt.info (options <**> Opt.helper) Opt.idm
   in case Opt.execParserPure Opt.defaultPrefs opts args of
        Opt.Success o -> Right o
        Opt.Failure help -> Left $ Parse $ toText $ fst $ Opt.renderFailure help progName
        Opt.CompletionInvoked _ -> Left CompletionInvoked

-- | Get a readable message out of the 'Error'
render :: Error -> Text
render (Parse msg) = msg
render CompletionInvoked = "Completion is not handled"

-- | The description of the command line options
options :: Opt.Parser Options
options =
  Options
    <$> Opt.option
      (Opt.maybeReader parseSomeFile)
      ( Opt.long "input"
          <> Opt.metavar "INPUT"
          <> Opt.help "Input file"
      )
    <*> Opt.option
      (Opt.maybeReader parseSomeDir)
      ( Opt.long "output"
          <> Opt.metavar "OUTPUT"
          <> Opt.help "Output directory"
      )
