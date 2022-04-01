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
--import System.IO


    -- Actions
import XMonad.Actions.UpdatePointer
import XMonad.Actions.FindEmptyWorkspace



    -- Hooks
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks


    -- Layouts
import XMonad.Layout.Spiral
import XMonad.Layout.Accordion
import XMonad.Layout.Tabbed



    -- Layouts modifiers
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.NoBorders
import XMonad.Layout.ShowWName



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
	--					--
	--	myColor Gruvbox			--
	--					--
	------------------------------------------

black 		= "#1d2021"
white 		= "fbf1c7"
grey 		= "928374"
red 		= "#cc241d"
green 		= "#98971a"
yellow 		= "#d79921"
blue 		= "#458588"


	------------------------------------------
	--					--
	--	myColor GENERAL			--
	--					--
	------------------------------------------

background 	= "#222526"
foreground 	= "#dfbf8e"
black 		= "#665c54"
red 		= "#ea6962"
green 		= "#a9b665"
yellow		= "#e78a4e"
blue 		= "#7daea3"
magenta 	= "#d3869b"
cyan 		= "#89b482"
white 		= "#dfbf8e"
altblack 	= "#928374"
altred 		= "#ea6962"
altgreen 	= "#a9b665"
altyellow 	= "#e3a84e"
altblue 	= "#7daea3"
altmagenta 	= "#d3869b"
altcyan 	= "#89b482"
altwhite 	= "#dfbf8e"


    -----------------------------------------------------------------------
    -- myLayout 	= valueXMonad-Contrib                           --
    -----------------------------------------------------------------------

mySpaceLayout 	= spacingWithEdge 20

myGaps		= gaps [(U,20), (R,20)]

layout  	= avoidStruts (spiral (6/7) ||| Accordion ||| Full)

myLayoutHook 	= myGaps
		$ mySpaceLayout 
	     	$ layout


    -----------------------------------------------------------------------
    --autoStartApp && ShowNameWorkSpaces && Cursor				                        --
    -----------------------------------------------------------------------

myStartupHook = do
	spawnOnce "nitrogen --restore"
	spawnOnce "picom -f"
	spawnOnce "dunst"
	setDefaultCursor xC_left_ptr

myCursor = updatePointer (0.5, 0.5) (0, 0) -- exact centre of window



myShowName :: SWNConfig
myShowName = def
    { swn_font              = "xft:JetBrains Mono:style=Italic:size=50"
    , swn_fade              = 1.0
    , swn_bgcolor           = black
    , swn_color             = blue
    }

    -----------------------------------------------------------------------
    -- keyBaddings				                        --
    -----------------------------------------------------------------------

myKeys = 
	 [ ("M-w", spawn myBrowser)
	 , ("M-r", spawn myFileManager  )
	 , ("M-c", spawn myEditor)
	 , ("M-d", spawn myLuncher)
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
         } >> myCursor
      }








    -----------------------------------------------------------------------
    -- callFunction XMonad  = myVariable	                        --
    -----------------------------------------------------------------------

myConfig = def  {modMask 		= myModMask
		, terminal		= myTerminal
		, borderWidth		= myBorderWidth
		, focusedBorderColor	= myFocusColor
		, normalBorderColor	= myNormColor
		, workspaces		= myWorkSpaces
		, layoutHook		= showWName' myShowName $ myLayoutHook
		, startupHook		= myStartupHook
	--	, logHook		= myCursor
		}`additionalKeysP` myKeys


