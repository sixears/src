{-# LANGUAGE TemplateHaskell #-}

import  qualified  Data.Map  as  M
  
import  Data.List    ( intercalate, lines    )
import  Data.Maybe   ( fromJust              )
import  System.IO    ( readFile              )
import  Text.Printf  ( printf                )

import  Fluffy.Data.Fractional  ( (//) )
import  Fluffy.Data.List        ( breakAt )
import  Fluffy.Data.Time        ( toHHMM  )
import  Fluffy.Getopt           ( ArgsCount( ArgsNone )
                                , mkOptSet, optBool, stdopts )

battPath = "/sys/bus/acpi/drivers/battery/PNP0C0A:00/power_supply/BAT0/uevent"

$( mkOptSet "opts" "OptionSet" "mkOptionSet" ArgsNone "" "getOpts"
            ([ optBool "raw" "raw" "dump raw data" ]
            ++ stdopts)
 )

dumpBattStats :: String -> IO()
dumpBattStats battStr = do
  let battMap     =  M.fromList $ map (breakAt '=') $ lines battStr
      battVal k   =  fromJust $ M.lookup ("POWER_SUPPLY_" ++ k) battMap
      battValI k  =  read (battVal k) :: Integer
      battStatus  =  battVal  "STATUS"
      chargeDsgn  =  battValI "CHARGE_FULL_DESIGN" -- in uAh
      chargeFull  =  battValI "CHARGE_FULL"        -- in uAh
      chargeNow   =  battValI "CHARGE_NOW"         -- in uAh
      currentNow  =  battValI "CURRENT_NOW"        -- in uA
      pct         =  (100 * chargeNow) `quot` chargeFull
      battHealth  =  (100 * chargeFull) // chargeDsgn
      timeLeft    =  (3600 * chargeNow) `quot` currentNow
      (hh, mm)    =  toHHMM $ timeLeft
  putStr $ printf (intercalate "\t" [ "%s: %dh%02dm (%d%%)" 
                                    , "[health - %3.1f%% of %dmAh]" 
                                    , "current - %3.2fA\n"
                                    ])
                  battStatus hh mm pct battHealth 
                  (chargeDsgn `quot` 1000) 
                  (currentNow // 1000000)


main = do
  (opts, []) <- getOpts
  battStr <- readFile battPath
  if (raw opts)
    then putStr battStr
    else dumpBattStats battStr