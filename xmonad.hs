---------------------------------------------------------------------------
--                                                                       --
--     _|      _|  _|      _|                                      _|    --
--       _|  _|    _|_|  _|_|    _|_|    _|_|_|      _|_|_|    _|_|_|    --
--         _|      _|  _|  _|  _|    _|  _|    _|  _|    _|  _|    _|    --
--       _|  _|    _|      _|  _|    _|  _|    _|  _|    _|  _|    _|    --
--     _|      _|  _|      _|    _|_|    _|    _|    _|_|_|    _|_|_|    --
--                                                                       --
---------------------------------------------------------------------------
    -- Base
import XMonad

    -- Actions
import XMonad.Actions.UpdatePointer 
import XMonad.Actions.FindEmptyWorkspace

    -- Hooks
import XMonad.Hooks.DynamicLog    -- for xmobar 
import XMonad.Hooks.ManageDocks   -- for xmobar

    -- Layouts
import XMonad.Layout.Spiral
import XMonad.Layout.OneBig
import XMonad.Layout.Simplest

    -- Layouts modifiers
import XMonad.Layout.Spacing
import XMonad.Layout.ShowWName
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Renamed
import XMonad.Layout.NoBorders

    -- Utilities
import XMonad.Util.Run                          --for xmobar
import XMonad.Util.SpawnOnce                    --startApp
import XMonad.Util.Cursor                       --normalCursor 
import XMonad.Util.EZConfig (additionalKeysP)   --keybaidings
import XMonad.Util.Ungrab

    -----------------------------------------------------------------------
    -- myVariable 	= valueXMonad                                        --
    -----------------------------------------------------------------------
myModMask :: KeyMask
myModMask = mod1Mask        -- Sets modkey to super/windows key

myTerminal :: String
myTerminal = "urxvt"        -- Sets default terminal

myBorderWidth :: Dimension
myBorderWidth = 2           -- Sets border width for windows

myNormColor :: String       -- Border color of normal windows
myNormColor   = black       -- This variable is imported from Colors.THEME

myFocusColor :: String      -- Border color of focused windows
myFocusColor  = aqua      -- This variable is imported from Colors.THEME

myLuncher :: String
myLuncher = "rofi -show drun -show-icons"

myFileManager :: String
myFileManager	= "urxvt -e ranger"

myBrowser :: String
myBrowser = "firefox"  -- Sets qutebrowser as browser

myFont :: String
myFont = "xft:JetBrains Mono:regular:size=9:antialias=true:hinting=true"

myEditor :: String
myEditor = "code-oss"  -- Sets code as editor

myWorkSpaces = [" 1 ", " 2 ", " 3 ", " 4 ", " 5 ", " 6 ", " 7 ", " 8 ", " 9 "]
--myWorkSpaces = [" DEV ", " SYS ", " HTTP ", " MUSC ", " VIDO ", " DOC ", " CHAT ", " FUN ", " END "]
--myWorkSpaces = [" I ", " II ", " III ", " IV ", " V ", " VI ", " VII ", " VIII ", " IX "]


	------------------------------------------
	--				                    	--
	--	myColor Gruvbox	            		--
	--				                    	--
	------------------------------------------
black 		= "#1d2021"
white 		= "fbf1c7"
grey 		= "928374"
red 		= "#cc241d"
orange      = "#fb4934"
green 		= "#98971a"
yellow 		= "#d79921"
blue 		= "#458588"
aqua        = "#689d6a"

	------------------------------------------
	--				                    	--
	--	AutoStart Appliction && Cursor  	--
	--				                    	--
	------------------------------------------
myStartupHook = do
            spawnOnce "dunst"
            spawnOnce "picom -f"
            spawnOnce "nitrogen --restore"
            setDefaultCursor xC_left_ptr

myCursorMove = updatePointer (0.5, 0.5) (0, 0) -- exact centre of window

	------------------------------------------
	--				                    	--
	--	layout && Gaps && Name WS           --
	--				                    	--
	------------------------------------------

myGaps = spacingRaw False (Border 0 10 10 10) True (Border 10 10 10 10) True

mySpirals  = renamed [Replace "Spiral"]
            $ myGaps
            $ spiral (6/7)


myOnebig = renamed [Replace "Work"]
         $ OneBig (3/4) (3/4)

mySimpleList  = renamed [Replace "Staduy"]
                $ noBorders
                $ Simplest

myLayoutHook = mkToggle (NOBORDERS ?? FULL ?? EOT)
             $ avoidStruts (mySpirals ||| myOnebig ||| mySimpleList )

myShowName = def
    { swn_font              = "xft:JetBrains Mono:style=Bold:size=60"
    , swn_fade              = 1.0
    , swn_bgcolor           = black
    , swn_color             = green
    }


	------------------------------------------
	--				                    	--
	--	mykeyBaddings                 		--
	--				                    	--
	------------------------------------------
myKeys = 
	 [ ("M-w", spawn myBrowser)
	 , ("M-r", spawn myFileManager  )
	 , ("M-c", spawn myEditor)
	 , ("M-d", spawn myLuncher)
     , ("M-s", spawn "pavucontrol")
     , ("M-f", sendMessage $ Toggle FULL)

     , ("<Print>", unGrab >> spawn "scrot")
     , ("M-<Print>", unGrab >> spawn "scrot -s")
     , ("M-S-<Print>", unGrab >> spawn "scrot -u")

	 , ("M-e", viewEmptyWorkspace)
	 , ("M-g", tagToEmptyWorkspace)

     , ("M-0", spawn "bash ~/myScripts/./myPowerMenu.sh")
     , ("M-S-w", spawn "bash ~/myScripts/./myWifiMenu.sh")
	 ]

	------------------------------------------
	--				                    	--
	--	APPlicyXMonad && Xmobar	      		--
	--				                    	--
	------------------------------------------
main = do
	xmproc <- spawnPipe "xmobar ~/.config/xmobar/xmobarrc.hs"
	xmonad $ docks myConfig {
	    logHook = dynamicLogWithPP xmobarPP {
                                              ppOutput = hPutStrLn xmproc
                                            , ppCurrent = xmobarColor "#689d6a" "" . wrap "[" "]"
                                            , ppTitle = xmobarColor "#b8bb26" "" . shorten 40
                                            , ppSep = " | "
		                                    , ppHiddenNoWindows = xmobarColor "#98971a" ""
		                                    , ppHidden = xmobarColor "#ebdbb2" ""
		                                    , ppVisible = xmobarColor yellow ""
                                            } >> myCursorMove
                                    }


	------------------------------------------
	--				                    	--
	--	callFunction XMonad  = myVariable   -- 
	--				                    	--
	------------------------------------------
myConfig = def  { modMask 		                = myModMask
		        , terminal		                = myTerminal
		        , borderWidth		            = myBorderWidth
		        , focusedBorderColor	        = myFocusColor
		        , normalBorderColor	            = myNormColor
		        , workspaces		            = myWorkSpaces
		        , layoutHook		            = showWName' myShowName $ myLayoutHook
		        , startupHook		            = myStartupHook
		        }`additionalKeysP` myKeys

