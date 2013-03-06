import XMonad

-- for xmobar
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run (spawnPipe)

import qualified Data.Map as M
import Data.List (sort)
import System.IO (hPutStrLn)

-- for dynamic workspaces
import qualified XMonad.StackSet as W
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.CopyWindow (copy)

-- for prompts (used by various different features)
import XMonad.Prompt
import XMonad.Prompt.Input
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
--  test out multihead support
--  test out different layouts
--  color scheme (similar to my current wmii color scheme)
--
--  refactor
--  reorder code blocks (main currently stuck in the middle)
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
-- Log Hook related stuff
---------------------------------------------------
--
-- This allows us to have multiple dynamicLogWithPP's. The main reason to do
-- this is so we can have multiple xmobars (e.g: one up top and one at the
-- bottom)
--
-- Note: I have no idea whether this is broken/buggy in some subtle way, as my
-- Haskell (and particularly xmonad) experience is limited. In particular, I'm
-- worried about the return / effect for X (). That being said, its currently
-- working, so it can't be that bad.
--
-- My understanding is X () means that it effects the X monad but has no
-- return. Ie: something on the screen changes.
multiDynamicLogWithPP :: [PP] -> X ()
-- No PP, so don't do anything
multiDynamicLogWithPP [] = return ()
multiDynamicLogWithPP [pp] = do
	dynamicLogWithPP pp
multiDynamicLogWithPP (pp:pps) = do
	dynamicLogWithPP pp
	multiDynamicLogWithPP pps

-- Below are my PP's for my top and bottom xmobars. Since I run xmobar via a
-- proc, and the procs are started in main and thus only in scope there, we
-- pass them in as parameters.
--
-- I'm using ppOrder (order of workspaces, layout, title, and others) to remove
-- components that I don't want shown in the particular xmobar. Much simpler
-- than blanking out particular components.


-- contains the window title info, but not much else
xmobarTopPP proc = xmobarPP
	{ ppOutput = hPutStrLn proc
	, ppTitle = xmobarColor "white" ""
	, ppOrder = \(_:_:title:_) -> [title]
	}

-- contains the layout and other extraneous info, but no title
xmobarBottomPP proc = xmobarPP
	{ ppOutput = hPutStrLn proc
	, ppOrder = \(workspaces:layout:_:other) -> (workspaces:layout:other)
	}




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
-- Action prompt
---------------------------------------------------
--
-- like wmii's action list, which had special commands to give the computer
-- much like running normal programs. E.g: shutdown, etc.
--
-- @see http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Prompt-Input.html
--
-- There may be some modules provided which already do some of this
-- functionality, but I didn't see it when implementing it.

-- mapping from actions to "commands" to execute
-- User configurable
--
-- TODO: make this a Map?
myActions = [ ("poweroff", "gksudo poweroff")
	, ("reboot", "gksudo reboot")
	, ("test", "xmessage -default okay 'this is a test action'")
	]

-- list of actions above, for use in inputPrompt completion
actionComplList = map fst myActions

-- does the specified action
-- TODO: better function name?
doAction :: String -> X()
doAction action =
	case actionToCommand action of
		Nothing      -> return ()
		Just command -> spawn $ command

-- takes a specified action and returns the "command" that would need to be run
-- for the action to be completed. Think of command (here) to be whatever you'd
-- need to type into a shell to do it.
actionToCommand :: String -> Maybe String
actionToCommand action = lookup action myActions


-- an input prompt which can do the actions specified in actions.
actionPrompt :: XPConfig -> X()
actionPrompt config = inputPromptWithCompl config "" (mkComplFunFromList actionComplList) ?+ doAction



---------------------------------------------------
-- main
---------------------------------------------------
--
-- Starts up all the necessary processes
-- Then starts up xmonad with our modified version of the defaultConfig
--
main = do
	-- make sure that there's spaces in the concating of the string, so as
	--  to prevent program args from running together
	xmobarTopProc <- spawnPipe $ "xmobar"
			++ " --bgcolor=" ++ systemFGColor
			++ " --fgcolor=" ++ lightGrey
			++ " ~/.xmonad/xmobar_top_rc"
	xmobarBottomProc <- spawnPipe $ "xmobar"
			++ " --bgcolor=" ++ systemFGColor
			++ " --fgcolor=" ++ lightGrey
			++ " ~/.xmonad/xmobar_bottom_rc"
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
		---  xmobars
		--   see above for def of xmobarXPP's
		--
		, logHook = multiDynamicLogWithPP
				[ xmobarTopPP xmobarTopProc
				, xmobarBottomPP xmobarBottomProc
				]
		}



---------------------------------------------------
-- custom keybindings
---------------------------------------------------
--  TODO: clean up
--
myKeys conf @(XConfig {XMonad.modMask = myModMask}) = M.fromList $
	-- program spawning
	[
	  ((myModMask, xK_p), shellPrompt myXPConfig)	-- like dmenu, but fits better with the rest of the theme
	, ((myModMask, xK_a), actionPrompt myXPConfig)	-- like wmii's action list (special comands to give the computer. E.g: shutdown, etc.)
	]
	++

	-- dynamic workspaces
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


