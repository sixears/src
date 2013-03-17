{-# LANGUAGE TemplateHaskell #-}

-- XX errorf for printf; see case [] below
-- XX exception type for read
-- XX exception handling without IO
-- XX extendable options

-- XX handle mini pages by opten
-- XX option to use unix find
-- XX getarg shouldn't be doing the find!
-- XX error exitval (with dry-run); see case [] below
-- XX testing for tr, list utils
-- XX async
-- XX inbuilt find
-- XX better arg count checking
-- XX generic make tool

--------------------------------------------------------------------------------

import Control.Monad          ( liftM, unless )
import Data.List              ( intercalate, isPrefixOf, partition )
import Data.Maybe             ( fromJust )
import Data.Time.Clock        ( diffUTCTime, getCurrentTime )
import System.Environment     ( getArgs )
import System.FilePath        ( (<.>), takeFileName )
import Text.Printf            ( printf )

----------------------------------------

import Fluffy.Cmd.HDoc     ( ghc_doc_top, home )
import Fluffy.Getopt       ( ArgsCount( ArgsMaybe )
                           , mkOptSet, stdoptsDR
                           )
import Fluffy.Sys.Log      ( debug, info, timeit, timing, warn )
import Fluffy.Sys.Process  ( rawSystemCheckExit, runChecked    )
import Fluffy.Text.String  ( tr, trOptions, lc                 )
import Fluffy.Tools.Find   ( FileFindOpts( None )
                           , filefind )

--------------------------------------------------------------------------------

find        = "/usr/bin/find"
firefox     = "/usr/bin/firefox"

finddoc :: String -> IO FilePath
finddoc name = if "/" `isPrefixOf` name then return name else finddoc_ name
finddoc_ name = do
  let expect = lc (tr "." "-" name) <.> "html"
  fns <- filefind None (\ fn _ -> (expect == (lc (takeFileName fn))))
                       (\ dn _ -> (takeFileName dn) /= "src")
                       ghc_doc_top
  case fns of
    []     -> error $ printf "failed to find any files matching '%s' (%s)\n"
                             expect name
    x:y:xs -> error (printf "found too many files matching arg '%s':\n  %s" expect
                            (intercalate "\n  " fns))
    x:xs   -> return x
  -- return $ head fns

_finddoc name = do
  let arg      = tr "." "-" name
      findargs = [ghc_doc_top, "-type", "f", "-iname", (printf "*%s*" arg)]
  (out, err) <- runChecked find findargs
  let outlines     = lines out
      (full, mini) = partition (\p -> "mini_" `isPrefixOf` takeFileName p)
                               outlines
      fns          = if False then mini else mini
      fn           = case fns of
        []     -> error (printf "failed to find any files matching arg '%s'\n"
                                arg)
        x:y:xs -> error (printf "found too many files matching arg '%s':%s\n"
                                arg
                                (intercalate "\n  " fns))
        x:xs   -> x
  return fn

$( mkOptSet "opts" "OptionSet" "mkOptionSet"
            (ArgsMaybe home) "module name" "getOpts"
            stdoptsDR
 )

main = do
  (opts, arg:[]) <- getOpts
  let drPutStrLn =  putStrLn .((if (dryRun opts) then "DRY-RUN> " else "")++)
  fn             <- timeit "find" (finddoc $ arg)
  drPutStrLn $ printf "opening page %s" fn
  unless (dryRun opts)
         (rawSystemCheckExit [firefox, "--new-tab", fn])
