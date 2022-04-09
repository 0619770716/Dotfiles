-- Base.
import XMonad

-- Actions.
import XMonad.Actions.UpdatePointer
import XMonad.Actions.FindEmptyWorkspace

-- Hooks.
import XMonad.Hooks.ManageDocks
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
import XMonad.Util.SpawnOnce
import XMonad.Util.Cursor
import XMonad.Util.Ungrab


--------------------------------------------------------------                                             
-- myVariable =  myValue                                  ====
-- -----------------------------------------------------------
myModMask       = mod1Mask                         -- Sets Mod Key to alt/Super/Win/Fn.
myTerminal      = "urxvt"                          -- Sets default Terminal Emulator.
myBorderWidth   = 2                                -- Sets Border Width in pixels.
myNormalColor   = "#1d2021"                        -- Border color of normal windows.
myFocusedColor  = "#b16286"                        -- Border color of focused windows.
--myWorkspaces    = [" 1 ", " 2 ", " 3 ", " 4 ", " 5 ", " 6 ", " 7 ", " 8 ", " 9 "] 
myWorkspaces = [" DEV ", " SYS ", " HTTP ", " MUSC ", " VEDO ", " CHAT ", " DOC ", " END "]

---------------------------------------------------------------------
-- Startup Applications                                            ==
---------------------------------------------------------------------
myStartupHook = do
    spawnOnce "nitrogen --restore &"                                            -- feh is the alternative "feh --bg-scale /directory/of/desired/background &"
    spawnOnce "picom --experimental-backends &"                                 --Compositor
    spawnOnce "dunst &"
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
myKeys = [ ("M-w",          spawn "firefox"                         )
         , ("M-c",          spawn "code-oss"                        )
         , ("M-s",          spawn "pavucontrol"                     )
         , ("M-r",          spawn "urxvt -e ranger"                 )
         , ("M-d",          spawn "rofi -show drun -show-icons"     )

         , ("<Print>",      unGrab >> spawn "scrot -F ~/myPics/myScreenShooter/%Y-%m-%d-%T-screenshot.png"   )   
         , ("S-<Print>",    unGrab >> spawn "scrot -s -F ~/myPics/myScreenShooter/%Y-%m-%d-%T-screenshot.png")                                                    
         , ("M-<Print>",    spawn "scrot -u -F ~/myPics/myScreenShooter/%Y-%m-%d-%T-screenshot.png"          )

         , ("M-f",          sendMessage $ Toggle FULL               )
         , ("M-e",          viewEmptyWorkspace                      )
         , ("M-g",          tagToEmptyWorkspace                     )

         , ("M-S-w",       spawn "bash ~/myScripts/myRofi/myWifiMenu.sh" )
         , ("M-0",         spawn "bash ~/myScripts/myRofi/myPowerMenu.sh")
         ]

----------------------------------------------------------------------------------------------------------
--               Applicy Functions && Xmobar                                                            ==
----------------------------------------------------------------------------------------------------------
xmobarTitleColor = "#C678DD"
xmobarCurrentWorkspaceColor = "#51AFEF"        
main    = do
        xmobar <- spawnPipe "xmobar ~/.config/xmobar/xmobarrc.hs"
        xmonad $ docks myConfig {
        logHook = dynamicLogWithPP  xmobarPP { ppOutput = hPutStrLn xmobar 
                                             , ppTitle = xmobarColor "#C678DD" "" . shorten 50                                                          --  Active window title.
                                             , ppCurrent = xmobarColor "#b16286" "" . wrap "<box type=Bottom width=2 mb=2 color=#b16286>" "</box>"      --  Current Layout Indicator.
                                             , ppHiddenNoWindows = xmobarColor "#C678DD" ""                                                             --  Hidden Workspaces without windows.
                                             , ppHidden = xmobarColor "#6745c2" "" . wrap "<box type=Top width=2 mt=2 color=#6745c2>" "</box>"          --  Hidden Workspaces.
                                             , ppVisible = xmobarColor "#51AFEF" ""                                                                     --  Visible Workspaces.
                                             , ppUrgent = xmobarColor "#C678DD" ""                                                                      --  Urgent workspace.
                                             , ppLayout = xmobarColor "#ebdbb2" ""                                                                      -- Current Layout Indicator.
                                             , ppSep = "  :  "                                                                                          --  Separator character.
                                             } >> updatePointer (0.75, 0.75) (0.75, 0.75) -- exact centre of window
                                    } 

----------------------------------------------------------------------------------------------------------------------
--               Call Functions = myVariable                                                                        ==
----------------------------------------------------------------------------------------------------------------------
myConfig = def {  modMask                   = myModMask
                , terminal                  = myTerminal
                , borderWidth               = myBorderWidth
                , focusedBorderColor        = myFocusedColor
                , normalBorderColor         = myNormalColor
                , workspaces                = myWorkspaces
                , startupHook               = myStartupHook
                , layoutHook                = showWName' myShowWNameTheme $ myLayoutHook
                } `additionalKeysP` myKeys
