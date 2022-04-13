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
myNormalColor   = "#282828"                        -- Border color of normal windows.
myFocusedColor  = "#d8e49c"                        -- Border color of focused windows.
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
myGaps          = spacingRaw False (Border 10 10 10 10) True (Border 20 20 20 20) True


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

         , ("<Print>",      unGrab >> spawn "scrot -F ~/myPics/myScreen/%Y-%m-%d-%T-screenshot.png"   )   
         , ("S-<Print>",    unGrab >> spawn "scrot -s -F ~/myPics/myScreen/%Y-%m-%d-%T-screenshot.png")                                                    
         , ("M-<Print>",    spawn "scrot -u -F ~/myPics/myScreen/%Y-%m-%d-%T-screenshot.png"          )

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
        logHook = dynamicLogWithPP  xmobarPP { ppOutput =  hPutStrLn xmobar                      
                                             , ppCurrent = xmobarColor "#b8bb26" "" . wrap "<box type=Bottom width=2 mb=2 color=#d8e49c>" "</box>"  
                                             , ppVisible = xmobarColor "#ebdbb2" ""                 
                                             , ppHidden = xmobarColor "#8ec07c" "" . wrap "<box type=Top width=2 mt=2 color=#ebdbb2>" "</box>" 
                                             , ppHiddenNoWindows = xmobarColor "#d79921" ""           
                                             , ppTitle = xmobarColor "#fabd2f" "" . shorten 60          
                                             , ppSep =  "<fc=#666666> <fn=0>|</fn> </fc>"                
                                             , ppLayout = xmobarColor "#ebdbb2" ""                          
                                             , ppUrgent = xmobarColor "#ebdbb2" "" . wrap "!" "!"                                                                 
                                             } >>  updatePointer (0.5, 0.5) (0, 0) -- exact centre of window
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
