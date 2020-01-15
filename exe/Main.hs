{-# LANGUAGE RecordWildCards #-}

module Main
  ( main,
  )
where

import Options.Applicative
import Protolude hiding (option)
import qualified Prove
import qualified Setup
import qualified Verify

-------------------------------------------------------------------------------
-- Command Line Interface
-------------------------------------------------------------------------------

data Entropy
  = Hardware
  | Software
  deriving (Eq, Show)

data Prover
  = Groth16
  | GrothMaller
  | Bulletproofs
  deriving (Eq, Show, Read)

data Command
  = Setup
      { circuitFile :: FilePath,
        entropySource :: Bool,
        prover :: Prover,
        outputDir :: FilePath,
        verbose :: Bool
      }
  | Prove
      { circuitFile :: FilePath,
        setupDir :: FilePath,
        inputFile :: FilePath,
        piFile :: FilePath
      }
  | Verify
      { setupDir :: FilePath,
        inputFile :: FilePath,
        piFile :: FilePath
      }
  deriving (Eq, Show)

setup :: Parser Command
setup = Setup <$> circuitInput <*> entropySource <*> prover <*> outputDir <*> verbose
  where
    circuitInput =
      strOption
        ( long "input"
            <> short 'i'
            <> metavar "FILE"
            <> help "Circuit input"
        )
    verbose =
      flag
        False
        True
        ( long "verbose"
            <> short 'v'
            <> help "Enable verbose mode"
        )
    prover =
      option
        auto
        ( long "prover"
            <> help "Groth16|Pinnochio|Bulletproofs"
            <> value Groth16
        )
    outputDir =
      strOption
        ( long "output"
            <> short 'o'
            <> metavar "DIR"
            <> help "output directory"
        )
    entropySource =
      flag
        False
        True
        ( long "hardware"
            <> help "Use hardware entropy source to generate common reference string"
        )

prove :: Parser Command
prove = Prove <$> circuitFile <*> setupDir <*> inputFile <*> piFile
  where
    circuitFile =
      strOption
        ( long "input"
            <> short 'i'
            <> metavar "FILE"
            <> help "program input"
        )
    setupDir =
      strOption
        ( long "setup"
            <> short 'd'
            <> metavar "DIR"
            <> help "setup directory"
        )
    inputFile =
      strOption
        ( long "inputs"
            <> short 'a'
            <> metavar "FILE"
            <> help "program inputs"
        )
    piFile =
      strOption
        ( long "pi"
            <> short 'p'
            <> metavar "FILE"
            <> help "proof output"
        )

verify :: Parser Command
verify = Verify <$> setupDir <*> inputFile <*> piFile
  where
    setupDir =
      strOption
        ( long "setup"
            <> short 'd'
            <> metavar "DIR"
            <> help "setup directory"
        )
    inputFile =
      strOption
        ( long "inputs"
            <> short 'a'
            <> metavar "FILE"
            <> help "program inputs"
        )
    piFile =
      strOption
        ( long "pi"
            <> short 'p'
            <> metavar "FILE"
            <> help "proof output"
        )

withInfo :: Parser a -> [Char] -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

sample :: Parser Command
sample =
  subparser $
    mconcat
      [ command "setup" (withInfo setup "Trusted Setup"),
        command "prove" (withInfo prove "Generate zero knowledge proof"),
        command "verify" (withInfo verify "Verify zero knowledge proof")
      ]

opts :: ParserInfo Command
opts =
  info
    (sample <**> helper)
    ( fullDesc
        <> progDesc "Generate zero knowledge proofs."
        <> header "Generate zero knowledge proofs. COMMANDS = setup | prove | verify"
    )

main :: IO ()
main = do
  options <- execParser opts
  print options
  case options of
    Setup {..} -> Setup.runSetup verbose entropySource circuitFile outputDir
    Prove {..} -> Prove.runProve circuitFile setupDir inputFile piFile
    Verify {..} -> Verify.runVerify setupDir inputFile piFile
