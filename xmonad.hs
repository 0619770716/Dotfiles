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
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

    -- Layouts
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed

    -- Layouts modifiers
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.NoBorders
import XMonad.Layout.ShowWName
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances

    -- Utilities
import XMonad.Util.SpawnOnce
import XMonad.Util.Cursor
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run 

    -----------------------------------------------------------------------
    -- myVariable 	= valueXMonad                                    --
    -----------------------------------------------------------------------
myModMask :: KeyMask
myModMask = mod1Mask        -- Sets modkey to super/windows key

myTerminal :: String
myTerminal = "urxvt"    -- Sets default terminal

myBorderWidth :: Dimension
myBorderWidth = 1           -- Sets border width for windows

myNormColor :: String       -- Border color of normal windows
myNormColor   = white   -- This variable is imported from Colors.THEME

myFocusColor :: String      -- Border color of focused windows
myFocusColor  = green     -- This variable is imported from Colors.THEME

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

-- myWorkSpaces = [" 1 ", " 2 ", " 3 ", " 4 ", " 5 ", " 6 ", " 7 ", " 8 ", " 9 "]
myWorkSpaces = [" DEV ", " SYS ", " HTTP ", " DOC ", " VBOX ", " CHAT ", " MUSC ", " VIDO ", " GFX "]

	------------------------------------------
	--				                    	--
	--	myColor Gruvbox	            		--
	--				                    	--
	------------------------------------------
black 		= "#1d2021"
white 		= "fbf1c7"
grey 		= "928374"
red 		= "#cc241d"
green 		= "#98971a"
yellow 		= "#d79921"
blue 		= "#458588"



    -----------------------------------------------------------------------
    -- myLayout 	= valueXMonad-Contrib                           --
    -----------------------------------------------------------------------
myGaps		    = gaps [(U,20), (R,20)]

mySpaceLayout 	= spacingWithEdge 20

layout  	    = mkToggle (NOBORDERS ?? FULL ?? EOT)
                $ avoidStruts (spiral (6/7) ||| simpleTabbed  )

myLayoutHook 	= mySpaceLayout 
                $ myGaps
	        	$ layout

myShowName :: SWNConfig
myShowName = def
    { swn_font              = "xft:JetBrains Mono:style=Italic:size=50"
    , swn_fade              = 1.0
    , swn_bgcolor           = black
    , swn_color             = blue
    }
    
    -----------------------------------------------------------------------
    --autoStartApp && Cursor    				                        --
    -----------------------------------------------------------------------
myStartupHook = do
	spawnOnce "nitrogen --restore"
	spawnOnce "picom -f"
	setDefaultCursor xC_left_ptr

myCursorMove = updatePointer (0.5, 0.5) (0, 0) -- exact centre of window

    -----------------------------------------------------------------------
    -- keyBaddings				                                           --
    -----------------------------------------------------------------------
myKeys = 
	 [ ("M-w", spawn myBrowser)
	 , ("M-r", spawn myFileManager  )
	 , ("M-c", spawn myEditor)
	 , ("M-d", spawn myLuncher)
     , ("M-f", sendMessage $ Toggle FULL)
	 , ("M-n", viewEmptyWorkspace)
	 , ("M-g", tagToEmptyWorkspace)
	 ]

    -----------------------------------------------------------------------
    -- APPlicyXMonad && Xmobar			                        --
    -----------------------------------------------------------------------
main = do
	xmproc <- spawnPipe "xmobar ~/.config/xmobar/xmobarrc.hs"
	xmonad $ ewmh $ docks myConfig {
	         logHook = dynamicLogWithPP xmobarPP {
                  ppCurrent = xmobarColor red "" . wrap "[" "]"
                , ppTitle = xmobarColor yellow "" . shorten 50
                , ppSep = "   "
                , ppOutput = hPutStrLn xmproc
		, ppHiddenNoWindows = xmobarColor green ""
		, ppHidden = xmobarColor blue ""
		, ppVisible = xmobarColor red ""
         } >> myCursorMove
      }

    -----------------------------------------------------------------------
    -- callFunction XMonad  = myVariable	                        --
    -----------------------------------------------------------------------
myConfig = def  { modMask 		        = myModMask
		        , terminal		        = myTerminal
		        , borderWidth		    = myBorderWidth
		        , focusedBorderColor	= myFocusColor
		        , normalBorderColor	    = myNormColor
		        , workspaces		    = myWorkSpaces
		        , layoutHook		    = showWName' myShowName $ myLayoutHook
		        , startupHook		    = myStartupHook
	--	        , logHook		= myCursor
		        }`additionalKeysP` myKeys

