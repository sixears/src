#!/usr/bin/env /home/martyn/bin/runghc

import Control.Monad         ( filterM, forM_, liftM, sequence_, when )
import Data.List             ( isPrefixOf, nub )
import Data.Maybe            ( fromJust, isJust )
import System.Environment    ( getArgs, getProgName )
import System.FilePath.Posix ( (</>), (<.>), takeFileName )
import System.Log.Logger     ( Priority(WARNING)
                             , setLevel, updateGlobalLogger
                             )
import System.Posix.Files    ( fileExist )
import System.Posix.Process  ( executeFile )

import qualified  Data.Tree  as  Tree

----------------------------------------

import Fluffy.Data.List              ( splitOn )
import Fluffy.Language.Haskell       ( imports )
import Fluffy.Language.Haskell.File  ( ghcompile, hs2o, module_hs )
import Fluffy.Sys.Env                ( envM )
import Fluffy.Sys.Exit               ( exitIORead )
import Fluffy.Sys.Log                ( info, parse_p_dflt, warn )
import Fluffy.Tools.Make             ( needsRemake )

--------------------------------------------------------------------------------

bin    = "/home/martyn/bin"
hlib   = bin </> "hlib"
runghc = "/usr/bin/runghc"

--------------------------------------------------------------------------------

-- set verbosity from envvar RUNHASKELL_VERBOSITY
setVerboseLogging :: String -> IO ()
setVerboseLogging envvar = do
  envval <- envM envvar
  when (isJust envval) 
       (parse_p_dflt (fromJust envval) ("env: " ++ envvar) WARNING
        >>= updateGlobalLogger "info" . setLevel)
  
_fi fn = do
  txt <- readFile fn
  let fluffies = filter (isPrefixOf "Fluffy.") (imports txt)
  return (fn, map (module_hs hlib) fluffies)

fluffyImportTree :: FilePath -> IO (Tree.Tree FilePath)
fluffyImportTree fn = Tree.unfoldTreeM _fi fn

fluffyImports :: FilePath -> IO [FilePath]
fluffyImports fn = do
  t <- fluffyImportTree fn
  info ("fluffyImports:\t" ++ (show (Tree.flatten t)))
  return $ Tree.flatten t

--------------------------------------------------------------------------------

main = do
  setVerboseLogging "RUNHASKELL_VERBOSITY"
  argv           <- getArgs
  prog_name      <- getProgName -- is basename of $0
  runHaskellName <- envM "RUNHASKELL_PROGNAME"
  let hs         = maybe (bin </> prog_name <.> "hs") 
                         (\x -> x <.> "hs")
                         runHaskellName
      args       =  ["-i" ++ hlib, hs] ++ argv
  hsExist        <- fileExist hs
  when (not hsExist)
    (sequence_ [ (warn ("no such file: '" ++ hs ++ "'"))
               , exitIORead
               ])
  info ("hs:\t" ++ hs)
  hs_text        <- readFile hs
  fluffy_fns     <- fluffyImports hs
  let flfs         =  map (\ f -> ([f], (hs2o f))) fluffy_fns
  flf_remakes      <- filterM (\ fs -> needsRemake (fst fs) (snd fs)) (nub flfs)
  -- always (re)compile the starting .hs file if there is compilation to be done
  -- but the .hs file isn't already listed
  let flf_remakes' =  if ((length flf_remakes) > 0) && ((fst $ head flf_remakes) /= [hs])
                      then ([hs], (hs2o hs)) : flf_remakes
                      else flf_remakes
  info ("flf_remakes':\t" ++ (show flf_remakes'))
  -- reverse to ensure compiles are done in the required order
  forM_ (reverse flf_remakes') compile_
  executeFile runghc False args Nothing
  where compile_ :: ([String], String) -> IO()
        compile_ ff = ghcompile (head $ fst ff) [hlib]

