module Main where
import CoreParser
import Pretty
import TM
import Translate
import Yul
import Options.Applicative

data Options = Options
    { input :: FilePath
    , contract :: String
    , output :: FilePath
    } deriving Show

optionsParser :: Parser Options
optionsParser = Options
    <$> argument str
        ( metavar "FILE" 
        <> help "Input file" )
    <*> strOption
        ( long "contract"
        <> short 'c'
        <> metavar "NAME"
        <> help "Contract name" 
        <> value "Output")
    <*> strOption
        ( long "output"
        <> short 'o'
        <> metavar "FILE"
        <> help "Output file" 
        <> value "Output.sol")
    

main :: IO ()
main = do
    options <- parseOptions
    src <- readFile (input options)
    let core = parseCore src
    {-
    putStrLn "/* Core:"
    putStrLn (render (nest 2 (pretty core)))
    putStrLn "*/"
    -}
    generatedYul <- runTM (translateCore core)
    let fooFun = wrapInSolFunction "main" generatedYul
    let doc = wrapInContract (contract options) "main()" fooFun
    -- putStrLn (render doc)
    putStrLn ("writing output to " ++ output options)
    writeFile (output options) (render doc)


parseOptions :: IO Options
parseOptions = execParser opts
  where
    opts = info (optionsParser <**> helper)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )
main9 :: IO ()
main9 = do
    src <- readFile "examples/01pairs.core"
    let core = parseCore src
    putStrLn "/* Core:"
    putStrLn (render (nest 2 (pretty core)))
    putStrLn "*/"
    generatedYul <- runTM (translateCore core)
    let fooFun = wrapInSolFunction "main" generatedYul
    let doc = wrapInContract "Foo" "main()" fooFun
    putStrLn (render doc)

