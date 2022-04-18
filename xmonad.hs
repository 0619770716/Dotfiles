import XMonad

-- Actions.
import XMonad.Actions.UpdatePointer
import XMonad.Actions.FindEmptyWorkspace

-- Hooks.
import XMonad.ManageHook
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks ( avoidStruts, docks )
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops 

--layout
import XMonad.Layout.OneBig
import XMonad.Layout.Spiral
import XMonad.Layout.Grid
import XMonad.Layout.CenteredMaster


-- Layouts/Modifiers.
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances

-- Utilities.
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.SpawnOnce ( spawnOnce )
import XMonad.Util.Cursor
import XMonad.Util.Ungrab

--------------------------------------------------------------                                             
-- myVariable =  myValue                                  ====
-- -----------------------------------------------------------
myModMask       = mod1Mask                         -- Sets Mod Key to alt/Super/Win/Fn.
myTerminal      = "urxvt"                          -- Sets default Terminal Emulator.
myBorderWidth   = 1                                -- Sets Border Width in pixels.
myNormalColor   = "#282828"                        -- Border color of normal windows.
myFocusedColor  = "#575268"                        -- Border color of focused windows.
--myWorkspaces  = [" 1 ", " 2 ", " 3 ", " 4 ", " 5 ", " 6 ", " 7 ", " 8 ", " 9 "] 
--myWorkspaces  = ["\xf269 ", "\xe61f ", "\xe795 ", "\xf121 ", "\xf419 ", "\xf308 ", "\xf74a ", "\xf7e8 ", "\xf827 "] 
myWorkspaces = [" DEV ", " SYS ", " HTTP ", " CHAT ", " DOC ", " END "]

---------------------------------------------------------------------
-- Startup Applications                                            ==
---------------------------------------------------------------------
myStartupHook = do
    spawnOnce "nitrogen --restore"                                            -- feh is the alternative "feh --bg-scale /directory/of/desired/background &"
    spawnOnce "picom --experimental-backends"                                 --Compositor
    spawnOnce "dunst"
    setDefaultCursor xC_left_ptr


----------------------------------------------------------------------------------
--layout XMonad                                                                 ==
----------------------------------------------------------------------------------
myGaps          = spacingRaw False (Border 0 0 0 0) True (Border 10 10 10 10) True

mySperial       = renamed [Replace "Sperial"]
                $ myGaps
                $ spiral (6/7)

myOneBig        = renamed [Replace "OneBig"]
                $ myGaps
                $ OneBig (3/4) (3/4)

myCenter        = renamed [Replace "Center"]
                $ myGaps
                $ centerMaster Grid

myLayoutHook    = mkToggle (NOBORDERS ?? FULL ?? EOT)
                $ avoidStruts ( mySperial ||| myCenter ||| myOneBig )

myShowWNameTheme = def
    { swn_font              = "xft:JetBrains Mono:bold:size=50"
    , swn_fade              = 1.0
    , swn_bgcolor           = "#1d1f1"
    , swn_color             = "#b16286"
    }

----------------------------------------------------------------------------------------------
--               keybidings                                                                 ==
----------------------------------------------------------------------------------------------
myKeys =            -- Lanuche Programme --
         [ ("M-w",          spawn "firefox"                         )
         , ("M-c",          spawn "code-oss"                        )
         , ("M-s",          spawn "pavucontrol"                     )
         , ("M-r",          spawn "urxvt -e ranger"                 )
         , ("M-d",          spawn "rofi -show drun -show-icons"     )
                    -- ScreenShoot --
         , ("<Print>",      unGrab >> spawn "scrot -F ~/pix/screen/%Y-%m-%d-%T-screenshot.png"   )   
         , ("S-<Print>",    unGrab >> spawn "scrot -s -F ~/pix/screen/%Y-%m-%d-%T-screenshot.png")                                                    
         , ("M-<Print>",    spawn "scrot -u -F ~/pix/screen/%Y-%m-%d-%T-screenshot.png"          )
                     -- import XMonad --
         , ("M-f",          sendMessage $ Toggle FULL               )
         , ("M-e",          viewEmptyWorkspace                      )
         , ("M-g",          tagToEmptyWorkspace                     )
                    -- myScripts --
         , ("M-S-w",       spawn "bash ~/scripts/rofi/wifiMenu.sh" )
         , ("M-0",         spawn "bash ~/scripts/rofi/powerMenu.sh")
         ]

------------------------------------------------------------------------------------------------------
--               ManageHook = dofloat, doCenterFloar, doShift                                       ==
------------------------------------------------------------------------------------------------------
myManageHook = composeAll
     [ className =? "mpv"               --> doCenterFloat
     , className =? "Sxiv"              --> doCenterFloat
     , className =? "Pavucontrol"       --> doCenterFloat
     , className =? "confirm"           --> doFloat
     , className =? "file_progress"     --> doFloat
     , className =? "dialog"            --> doFloat
     , className =? "download"          --> doFloat
     , className =? "error"             --> doFloat
     , className =? "Gimp"              --> doFloat
     , className =? "notification"      --> doFloat
     , className =? "pinentry-gtk-2"    --> doFloat
     , className =? "splash"            --> doFloat
     , className =? "toolbar"           --> doFloat
     , className =? "Yad"               --> doCenterFloat
     , className =? "mpv"               --> doCenterFloat
     , className =? "Sxiv"              --> doCenterFloat
     , className =? "Pavucontrol"       --> doCenterFloat
     , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
     , isFullscreen -->  doFullFloat
     ] 





----------------------------------------------------------------------------------------------------------------------
--               Call Functions = myVariable                                                                        ==
----------------------------------------------------------------------------------------------------------------------
main = do
    xmproc <- spawnPipe "xmobar ~/.config/xmobar/xmobarrc.hs"
    xmonad $ docks def { modMask                    = myModMask
                        , terminal                  = myTerminal
                        , borderWidth               = myBorderWidth
                        , focusedBorderColor        = myFocusedColor
                        , normalBorderColor         = myNormalColor
                        , workspaces                = myWorkspaces
                        , startupHook               = myStartupHook
                        , layoutHook                = showWName' myShowWNameTheme $ myLayoutHook
                        , manageHook                = myManageHook
                        , logHook                   = dynamicLogWithPP xmobarPP 
                                                    { ppOutput = hPutStrLn xmproc 
                                                    , ppCurrent = xmobarColor "#C9CBFF" "" . wrap "<box type=Bottom width=3 mb=3 color=#96CDFB>" "</box>"  
                                                    , ppHidden = xmobarColor "#988BA2" "" . wrap "<box type=Top width=2 mt=2 color=#96CDFB>" "</box>" 
                                                    , ppHiddenNoWindows = xmobarColor "#6E6C7E" ""           
                                                    , ppVisible = xmobarColor "#ebdbb2" ""                 
                                                    , ppTitle = xmobarColor "#6E6C7E" "" . shorten 60          
                                                    , ppSep =  "<fc=#666666> <fn=0>|</fn> </fc>"                
                                                    , ppLayout = xmobarColor "#C9CBFF" ""                          
                                                    , ppUrgent = xmobarColor "#ebdbb2" "" . wrap "!" "!"                                                                 
                                                    } >>  updatePointer (0.5, 0.5) (0, 0) -- exact centre of window
                        } `additionalKeysP` myKeys
