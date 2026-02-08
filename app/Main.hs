-- |This module implements a command-line interface to the implementation of
-- Carneades. CAES + Haskell = caell
{-# LANGUAGE RecordWildCards #-}
module Main
  ( main
  ) where

import Language.CarneadesIntoDung.Translation
import Language.Carneades.CarneadesDSL (CAES(..), getAllArgs, applicable,
                                        getProps, acceptable)
import Language.Carneades.Input
import Language.Dung.AF (groundedExt)
import Language.Dung.Output

import Options.Applicative
import System.Exit
import Control.Monad (when)

data Options = Options
  { optCegartix       :: Bool
  , optLaxCegartix    :: Bool
  , optFileName       :: FilePath
  , optOutputFile     :: Maybe FilePath
  , optCorrespondence :: Bool
  , optExtension      :: Bool
  , optXSemantics     :: Bool
  } deriving (Show)

optionsParser :: Parser Options
optionsParser = Options
  <$> switch
        (  long "cegartix"
        <> help "Output in strict CEGARTIX/PrefSat format (standard)"
        )
  <*> switch
        (  long "lax-cegartix"
        <> help "Output in lax CEGARTIX/PrefSat format (+parentheses)"
        )
  <*> strOption
        (  long "filename"
        <> metavar "FILE"
        <> help "Name of the file to be read"
        )
  <*> optional (strOption
        (  long "outputfile"
        <> metavar "FILE"
        <> help "Name of the file to be written"
        ))
  <*> switch
        (  long "correspondence"
        <> help "Display satisfaction of correspondence result"
        )
  <*> switch
        (  long "extension"
        <> help "Output unique complete extension for the translated CAES"
        )
  <*> switch
        (  long "x-semantics"
        <> help "Output evaluation of the original Carneades semantics"
        )

opts :: ParserInfo Options
opts = info (optionsParser <**> helper)
  (  fullDesc
  <> progDesc "An implementation of Carneades in Haskell"
  <> header "caell - Carneades + Haskell argumentation tool"
  )

main :: IO ()
main = do
  options@Options{..} <- execParser opts
  input <- readFile optFileName
  caes <- case parseCAES input of
    Left err -> do
      putStrLn "Parsing error: "
      print err
      exitWith (ExitFailure 1)
    Right c -> return c
  exec options caes

-- |Execute supplied options
exec :: Options -> CAES -> IO ()
exec Options{..} caes@(CAES (argSet, (assumptions, _), _)) = do
  let args = getAllArgs argSet
  let transCaes = translate caes
  putStrLn $ "Argument set: " ++ show args
  when optXSemantics $ do
    putStrLn "Evaluation under original semantics:"
    putStrLn "Applicable arguments:"
    print (filter (`applicable` caes) args)
    putStrLn "Acceptable propositions:"
    print (filter (\p -> p `acceptable` caes
                      || p `elem` assumptions)
                  (getProps argSet))
  when optExtension $
    putStrLn "Extension after translation: "
      >> print (groundedExt transCaes)
  when optCorrespondence $
    putStrLn ("Correspondence of applicability is: " ++ show (corApp caes))
      >> putStrLn ("Correspondence of acceptability is: " ++ show (corAcc caes))
  let useStrict = optCegartix && not optLaxCegartix
  case optOutputFile of
    Nothing -> return ()
    Just fp -> do
      if useStrict
        then writeFile fp (toStrictCegartix transCaes)
        else writeFile fp (toCegartix transCaes)
      putStrLn "File outputted."
