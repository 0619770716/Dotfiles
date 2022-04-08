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

-- Layouts/Modifiers.
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
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
myWorkspaces    = [" 1 ", " 2 ", " 3 ", " 4 ", " 5 ", " 6 ", " 7 ", " 8 ", " 9 "] 

---------------------------------------------------------------------
-- Startup Applications                                            ==
---------------------------------------------------------------------
myStartupHook = do
    spawnOnce "nitrogen --restore"                  -- feh is the alternative "feh --bg-scale /directory/of/desired/background &"
    spawnOnce "picom -f"                            --Compositor
    spawnOnce "dunst"
    setDefaultCursor xC_left_ptr


----------------------------------------------------------------------------------
--layout XMonad                                                                 ==
----------------------------------------------------------------------------------
myGaps          = spacingRaw False (Border 0 0 0 0) True (Border 10 10 10 10) True

mySperial       = renamed [Replace "Sperial"]
                $ myGaps
                $ spiral (6/7)

myOneBig        = renamed [Replace "Work"]
                $ myGaps
                $ OneBig (3/4) (3/4)

myLayoutHook    = smartBorders
                $ mkToggle (NOBORDERS ?? FULL ?? EOT)
                $ avoidStruts ( mySperial ||| myOneBig )

----------------------------------------------------------------------------------------------
--               keybidings                                                                 ==
----------------------------------------------------------------------------------------------
myKeys = [ ("M-w",          spawn "firefox"                     )
         , ("M-c",          spawn "code-oss"                    )
         , ("M-s",          spawn "pavucontrol"                 )
         , ("M-r",          spawn "urxvt -e ranger"             )
         , ("M-d",          spawn "rofi -show drun"             )
         , ("<Print>",      unGrab >> spawn "scrot"             )   
         , ("M-<Print>",    unGrab >> spawn "scrot -e"          )   
         , ("M-S-<Print>",  unGrab >> spawn "scrot -u"          )
         , ("M-f",          sendMessage $ Toggle FULL           )
         , ("M-e",          viewEmptyWorkspace                  )
         , ("M-g",          tagToEmptyWorkspace                 )
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
                                             , ppTitle = xmobarColor xmobarTitleColor "" . shorten 50
                                             , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor "" . wrap "[" "]"
                                             , ppHiddenNoWindows = xmobarColor xmobarTitleColor ""
                                             , ppHidden = xmobarColor xmobarTitleColor "" 
                                             , ppVisible = xmobarColor xmobarCurrentWorkspaceColor "" 
                                             , ppUrgent = xmobarColor xmobarTitleColor "" 
                                             , ppSep = "  :  "
                                             } >> updatePointer (0.5, 0.5) (0, 0) -- exact centre of window
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
                , layoutHook                = myLayoutHook
                } `additionalKeysP` myKeys
