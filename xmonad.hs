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


--------------------------------------------------
-- TODO:
--  wmii like action keys (e.g: shutdown, reboot)?
--  configure xmobar
--  configure urxvt
--  test out multihead support
--  test out different layouts
--  see if we can get any good window decorators
--  color scheme

-- TODO:
--  refactor
--  extract constants



myModMask = mod1Mask


-- layouts
-- avoidStruts is for enabling docks (xmobar, dmenu)
-- TODO: IM layout?
myLayouts = Mirror tall ||| tall ||| StackTile 1 (3/100) (1/2) ||| Full ||| simpleTabbed
	where
		tall = Tall 1 (3/100) (1/2)

-- apply layout modifiers
myLayoutHook = avoidStruts $ myLayouts


-- workspaces
myWorkspaces = map show [0..9] ++ sort ["mail", "music", "upgrade", "im", "backup"]


-- main
main = do
	xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
	xmonad $ defaultConfig
		{
                  terminal    = "urxvt"
		, modMask     = myModMask
		, keys 	      = myKeys <+> keys defaultConfig

		-- aesthetics
		, borderWidth 	     = 3
		, normalBorderColor  = "#cccccc"
		, focusedBorderColor = "#cd8b00"		-- TODO: focused border color

		-- layouts and window management
		, manageHook = manageDocks <+> manageHook defaultConfig
		, layoutHook = myLayoutHook 
		, workspaces = myWorkspaces
 
		--  xmobar
		--   see also: .xmobarrc
		, logHook = dynamicLogWithPP xmobarPP
			{ ppOutput = hPutStrLn xmproc
			, ppTitle = xmobarColor "white" "" . shorten 50
			}
		}




	-----------------------
	-- custom keybindings
	-----------------------
	--  TODO: clean up
	--
myKeys conf @(XConfig {XMonad.modMask = myModMask}) = M.fromList $
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
	--  removes emptyworkspaces (so its more similar to wmii)
	--   doesn't remove workspaces in myWorkspaces (so only dynamically created ones)
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




-- my XMonad.Prompt configuration
myXPConfig = defaultXPConfig
	{
	  bgColor = "#000033"
	, position = Top
	}


