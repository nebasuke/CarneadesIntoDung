-- |This module implements a command-line interface to the implementation of 
-- Carneades. CAES + Haskell = caell
--
-- Code in this module partly taken from/inspired by Shinobu
-- See: http://zuttobenkyou.wordpress.com/2011/04/19/haskell-using-cmdargs-single-and-multi-mode/
-- and http://listx.github.com/
{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main
  (
    main
  )
 where
import Language.CarneadesIntoDung.Translation
import Language.Carneades.CarneadesDSL(CAES(..), getAllArgs, applicable,
                                       getProps, acceptable)
import Language.Carneades.Input
import Language.Dung.AF(groundedExt, preferredExt, stableExt, semiStableExt,
                        DungAF(..))
import Language.Dung.Output

import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)
import System.Exit
import Control.Monad (when, unless)

data MyOptions = MyOptions {
  cegartix          :: Bool,
  laxCegartix       :: Bool,
  fileName          :: String,
  outputFile        :: String,
  correspondence    :: Bool,
  extension         :: Bool,
  xSemantics :: Bool
 } deriving (Show, Data, Typeable)

myProgOpts :: MyOptions
myProgOpts = MyOptions
    { cegartix    = True     &= help "Output in strict CEGARTIX/PrefSat format (standard)" 
    , laxCegartix = False    &= help "Output in lax CEGARTIX/PrefSat format (+parentheses)" 
    , fileName   = def       &= typFile &= help "Name of the file to be read"
    , outputFile = def       &= typFile &= help "Name of the file to be written"
    , extension  = True      &= help "Output unique complete extension for the translated CAES (standard)"
    , correspondence = False &= help "Display satisfaction of correspondence result"
    , xSemantics = False     &= help "Output evaluation of the original Carneades semantics"
   }
 
getOpts :: IO MyOptions
getOpts = cmdArgs $ myProgOpts
    &= versionArg [explicit, name "version", name "v", summary _PROGRAM_INFO]
    &= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)
    &= help _PROGRAM_ABOUT
    &= helpArg [explicit, name "help", name "h"]
    &= program _PROGRAM_NAME
 
_PROGRAM_NAME = "caell"
_PROGRAM_VERSION = "1.0"
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_PROGRAM_ABOUT = "An implementation of Carneades in Haskell"
_COPYRIGHT = "(C) Bas van Gijzel 2014"


main :: IO ()
main = do 
        args <- getArgs
        opts <- (if null args then withArgs ["--help"] else id) getOpts
        optionHandler opts

-- |Check any malformed arguments/missing arguments. 
optionHandler :: MyOptions -> IO ()
optionHandler opts@MyOptions{..}  = do
    when (null fileName) $ putStrLn "--fileName is blank!" >> exitWith (ExitFailure 1)
    input <- readFile fileName
    let opts' = opts {cegartix = not laxCegartix}
    caes <- case parseCAES input of 
           Left err -> putStrLn "Parsing error: " >> print err >> exitWith (ExitFailure 1)
           Right caes -> return caes
    exec opts' caes

-- |Execute supplied options
exec :: MyOptions -> CAES -> IO ()
exec opts@MyOptions{..} caes@(CAES (argSet, (assumptions, _), _)) = do
    let args = getAllArgs argSet
    let transCaes = translate caes
    putStrLn $ "Argument set: " ++ show args
    when xSemantics $
      putStrLn "Evaluation under original semantics:" >>
      putStrLn "Applicable arguments:" >> 
      print (filter (`applicable` caes) args) >>
      putStrLn "Acceptable propositions:" >> 
      print (filter (\ p -> p `acceptable` caes 
                         || p `elem` assumptions)
                    (getProps argSet))  
    when extension $ putStrLn "Extension after translation: " 
      >> print (groundedExt transCaes)
    when correspondence $ putStrLn ("Correspondence of applicability is: " ++ show (corApp caes))
      >> putStrLn ("Correspondence of acceptability is: " ++ show (corAcc caes))
    unless (null outputFile)
      $ if cegartix 
          then writeFile outputFile (toStrictCegartix transCaes) >> putStrLn "File outputted."
          else writeFile outputFile (toCegartix transCaes) >> putStrLn "File outputted."