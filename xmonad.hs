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
myBorderWidth = 2           -- Sets border width for windows


myNormColor :: String       -- Border color of normal windows
myNormColor   = "#dddddd"   -- This variable is imported from Colors.THEME


myFocusColor :: String      -- Border color of focused windows
myFocusColor  = "#00FF00"     -- This variable is imported from Colors.THEME

myLuncher :: String
myLuncher = "rofi -show drun -show-icons"

myFileM	= "urxvt -e ranger"

myBrowser :: String
myBrowser = "firefox"  -- Sets qutebrowser as browser

myFont :: String
myFont = "xft:JetBrains Mono:regular:size=9:antialias=true:hinting=true"


myEditor :: String
myEditor = "code-oss"  -- Sets code as editor


-- myWorkSpaces = [" 1 ", " 2 ", " 3 ", " 4 ", " 5 ", " 6 ", " 7 ", " 8 ", " 9 "]
myWorkSpaces = [" dev ", " www ", " sys ", " doc ", " vbox ", " chat ", " mus ", " vid ", " gfx "]


    -----------------------------------------------------------------------
    -- myLayout 	= valueXMonad-Contrib                           --
    -----------------------------------------------------------------------

mySpaceLayout 	= spacingWithEdge 20

layout  	= smartBorders
		$ mkToggle (NOBORDERS ?? FULL ?? EOT)	
		$ spiral (6/7) ||| Accordion ||| Full

myLayoutHook 	= mySpaceLayout 
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
    , swn_bgcolor           = "#1c1f24"
    , swn_color             = "#ffffff"
    }

    -----------------------------------------------------------------------
    -- keyBaddings				                        --
    -----------------------------------------------------------------------

myKeys = 
	 [ ("M-w", spawn myBrowser)
	 , ("M-r", spawn myFileM  )
	 , ("M-c", spawn myEditor)
	 , ("M-d", spawn myLuncher)
	 , ("M-s", sendMessage $ Toggle FULL) 
	 , ("M-n", viewEmptyWorkspace)
	 , ("M-g", tagToEmptyWorkspace)
	 ]




    -----------------------------------------------------------------------
    -- APPlicyXMonad && Xmobar			                        --
    -----------------------------------------------------------------------
xmobarTitleColor = "#C678DD"
xmobarCurrentWorkspaceColor = "#51AFEF"


main = do
	xmproc <- spawnPipe "xmobar ~/.config/xmobar/xmobar-material.hs"
	xmonad $ ewmh $ docks myConfig {
	         logHook = dynamicLogWithPP xmobarPP {
                  ppCurrent = xmobarColor xmobarCurrentWorkspaceColor "" . wrap "[" "]"
                , ppTitle = xmobarColor xmobarTitleColor "" . shorten 50
                , ppSep = "   "
                , ppOutput = hPutStrLn xmproc
		, ppHiddenNoWindows = xmobarColor xmobarTitleColor ""
         } >> updatePointer (0.75, 0.75) (0.75, 0.75)
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

