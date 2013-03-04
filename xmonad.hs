import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run (spawnPipe)

import qualified Data.Map as M
import Data.List (sort)
import System.IO

-- for dynamic workspaces
import qualified XMonad.StackSet as W
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.CopyWindow(copy)
import XMonad.Prompt
import XMonad.Prompt.Shell

-- switching to next/prev workspace
import XMonad.Actions.CycleWS

-- layouts
import XMonad.Layout.StackTile
import XMonad.Layout.Tabbed

-- layout modifiers


---------------------------------------------------
-- General TODOs
---------------------------------------------------
--  wmii like action keys (e.g: shutdown, reboot)?
--  configure xmobar
--  test out multihead support
--  test out different layouts
--  see if we can get any good window decorators
--  color scheme (similar to my current wmii color scheme)
--
--  refactor
--  extract constants



---------------------------------------------------
-- Constants
---------------------------------------------------

--
--- color definitions
--
-- grays
white 	   	= "#ffffff"
darkGrey   	= "#222222"
mediumGrey 	= "#777777"
lightGrey  	= "#cccccc"

-- yellows
bronze 		= "#cd8b00"

-- blues
wmiiBlue 	= "#285577"
darkBlue 	= "#000033"


--
--- general configurations
--
myTerminal 	= "urxvt"
myModMask 	= mod1Mask


--
--- aesthetics
--
systemBGColor 	= lightGrey
systemFGColor 	= wmiiBlue

myBorderWidth 		= 3
myNormalBorderColor 	= lightGrey
myFocusedBorderColor 	= bronze



---------------------------------------------------
-- layouts
---------------------------------------------------
-- avoidStruts is for enabling docks (xmobar, dmenu)
-- TODO: IM layout?
myLayouts = Mirror tall ||| tall ||| StackTile 1 (3/100) (1/2) ||| Full ||| simpleTabbed
	where
		tall = Tall 1 (3/100) (1/2)


-- apply layout modifiers
myLayoutHook = avoidStruts $ myLayouts





---------------------------------------------------
-- workspaces
---------------------------------------------------
myWorkspaces = map show [0..9] ++ sort ["mail", "music", "upgrade", "im", "backup"]


-- manage particular windows
--  send them to particular workspaces or make the float, etc.
myManageHook = composeAll . concat $
	[]



---------------------------------------------------
-- startup programs
-- see: .xsession and .Xinitrc
--
-- I'm not going to run programs on startup from my xmonad.hs (for the time
-- being), b/c then whenever I reloaded xmonad (e.g: when testing out a
-- configuration change), it'd end up also starting up those programs. Which means
-- I'd end up with extra running copies, which I don't want.
---------------------------------------------------



---------------------------------------------------
-- main
---------------------------------------------------
main = do
	-- make sure that there's spaces in the concating of the string, so as
	--  to prevent program args from running together
	xmproc <- spawnPipe $ "xmobar"
			++ " --bgcolor=" ++ systemFGColor
			++ " --fgcolor=" ++ lightGrey
			++ " ~/.xmonad/xmobarrc"
	xmonad $ defaultConfig
		{
                  terminal    = myTerminal
		, modMask     = myModMask
		, keys 	      = myKeys <+> keys defaultConfig

		--
		--- aesthetics
		--
		, borderWidth 	     = myBorderWidth
		, normalBorderColor  = myNormalBorderColor
		, focusedBorderColor = myFocusedBorderColor

		--
		--- layouts and window management
		--
		, manageHook = myManageHook <+> manageDocks <+> manageHook defaultConfig
		, layoutHook = myLayoutHook 
		, workspaces = myWorkspaces

		--
		---  xmobar
		--   see also: .xmobarrc
		--
		-- TODO: configuration (particularly colors, etc.)
		--
		, logHook = dynamicLogWithPP xmobarPP
			{ ppOutput = hPutStrLn xmproc
			, ppTitle = xmobarColor "white" "" . shorten 50
			}
		}



---------------------------------------------------
-- custom keybindings
---------------------------------------------------
--  TODO: clean up
--
myKeys conf @(XConfig {XMonad.modMask = myModMask}) = M.fromList $
	-- program spawning
	[
--	  ((myModMask, xK_p), spawn "exe=`dmenu_path | dmenu -nb '#000033' -nf grey` && eval \"exec $exe\"")    -- Launch dmenu with our colors 
	  ((myModMask, xK_p), shellPrompt myXPConfig)	-- like dmenu, but fits better with the rest of the theme

	]
	++

	-- dynamic workspaces
	-- TODO: configure Prompt
	[
	  ((myModMask .|. shiftMask, xK_BackSpace), removeEmptyWorkspace)							-- remove current workspace
	, ((myModMask              , xK_t        ), removeEmptyWorkspaceAfterExcept myWorkspaces (selectWorkspace myXPConfig))	-- select workspace
	, ((myModMask .|. shiftMask, xK_t        ), withWorkspace myXPConfig (windows . W.shift))				-- shift window to workspace
	]
	++

	-- modified workspace switching keybindings
	--  adds a workspace 0
	--  removes empty workspaces (so its more similar to dynamic workspaces/tags in wmii)
	--   doesn't remove workspaces in myWorkspaces (so only dynamically created ones are removed)
	[((m .|. myModMask, k), removeEmptyWorkspaceAfterExcept myWorkspaces (windows $ f i))
	    | (i, k) <- zip (XMonad.workspaces conf) ([xK_0 .. xK_9])
	    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
	]
	++

	-- a basic CycleWS setup
	--  allows cycling through the workspaces in order
	[
	-- main keybindings
	  ((myModMask .|. shiftMask, xK_m), moveTo Next NonEmptyWS)
	, ((myModMask .|. shiftMask, xK_n), moveTo Prev NonEmptyWS)
	-- alternate keybindings (includes moving windows as well)
	, ((myModMask              , xK_Right), moveTo Next NonEmptyWS)
	, ((myModMask              , xK_Left), moveTo Prev NonEmptyWS)
	, ((myModMask .|. shiftMask, xK_Right), shiftToNext >> nextWS)
	, ((myModMask .|. shiftMask, xK_Left), shiftToPrev >> prevWS)
	]

	-- Example logging keybinding
--	[
--	  ((myModMask, xK_o), spawn ("echo \'" ++ show (XMonad.workspaces conf ) ++ "\' > /tmp/workspaces"))
--	]
--	++




---------------------------------------------------
-- XMonad.Prompt configuration
---------------------------------------------------
--
-- I think alwaysHighlight will cause the prompts to be like in wmii (e.g:
-- dmenu, tab switching) where the first autocompletion is automatically
-- highlighted. So that simply pressing enter then results in this autocompletion
-- to be selected. Currently, I have to press an extra tab to run a program.
--
-- Note: it appears this feature is in xmonad 0.11 but not 0.10. I'm currently
-- running 0.10 (debian testing's package), and it fails to compile when I try
-- to use it. Error given is: "'alwaysHighlight' is not a (visible) constructor
-- field name"
--
-- Note: I don't like the autocompletion feature b/c it automatically selects
-- it and acts as if we press enter. A typo could easily lead to running god knows
-- what.
--
-- TODO: dimensions (to match xmobar)
myXPConfig = defaultXPConfig
	{
	  bgColor = wmiiBlue -- darkBlue
	, position = Top
--	, alwaysHighlight = True
	}


