import XMonad

-- for xmobar
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import System.IO (hPutStrLn)
import XMonad.Util.Run (spawnPipe)

-- for various things
import Data.List (sort, foldl')
import qualified Data.Map as M
import qualified XMonad.StackSet as W

-- for dynamic workspaces
import XMonad.Actions.CopyWindow (copy)
import XMonad.Actions.DynamicWorkspaces
import XMonad.Util.WorkspaceCompare (getSortByTag)     -- for keeping workspaces sorted

-- for prompts (used by various different features)
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.Shell

-- switching to next/prev workspace
import XMonad.Actions.CycleWS

-- layouts
import Data.Ratio ((%))
import XMonad.Layout.IM
import XMonad.Layout.PerWorkspace
import XMonad.Layout.StackTile
import XMonad.Layout.Tabbed

-- layout modifiers
import XMonad.Hooks.UrgencyHook        -- installs itself as a layout modifier

-- manage hooks
import XMonad.Hooks.ManageHelpers

-- keys
import Graphics.X11.ExtraTypes.XF86	-- special key sybmols

-- Pasting
import XMonad.Util.Paste




-- General TODOs
------------------------------------------------------------------------------
--  test out multihead support
--  test out different layouts
--  color scheme (similar to my current wmii color scheme)
--
--  extract constants
--  
--  make a custom keybindings file (using the svg provided for the defaults)
--
--
--  Leaving a workspace via cycling doesn't remove it if its empty (as mod-t, mod-[0..9] do)
--    Simply need to use removeEmptyWorkspaceAfterExcept?
--
--
--  XMobar workspace coloring
--   might be nice to have different colors for indicating info about the
--   workspace. Perhaps use only colors to indicate current workspace even. See
--   http://xmonad.org/xmonad-docs/xmonad-contrib/src/XMonad-Hooks-DynamicLog.html#xmobarPP
--   for the defaultPP for our xmobar. It shows how to do the coloring.
--
--   Things we could use coloring for:
--      current workspace
--      visible workspace (ie: Xinerama)
--      urgent workspace
--
--
--  dialogs popping up under other dialog boxes (floating windows)
--   it appears that my composeAllMaybe() trick wasn't enough, or I made a mistake somewhere
--   Good test program: truecrypt
--      starts as a dialog box, then can create another dialog box with "create volume"






------------------------------------------------------------------------------
-- Constants
------------------------------------------------------------------------------

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
myModMask 	= mod1Mask		-- TODO: try out mod4Mask (win key) at some point?


--
--- aesthetics
--
systemBGColor 	= lightGrey
systemFGColor 	= wmiiBlue

myBorderWidth 		= 3
myNormalBorderColor 	= lightGrey
myFocusedBorderColor 	= bronze









------------------------------------------------------------------------------
-- layouts
------------------------------------------------------------------------------
-- avoidStruts is for enabling docks (xmobar, trayer, etc.)
myLayouts = onWorkspace "im" im $
	    Mirror tall ||| tall ||| stack ||| Full ||| simpleTabbed		-- defaults
	where
		nmaster = 1
		delta 	= 3/100
		ratio 	= 1/2

		tall 	= Tall nmaster delta ratio
		stack 	= StackTile nmaster delta ratio
		im 	= gridIM (1%5) (Role "buddy_list")


-- apply layout modifiers
myLayoutHook = avoidStruts $ myLayouts








------------------------------------------------------------------------------
-- workspaces
------------------------------------------------------------------------------
myWorkspaces = map show [0..9] ++ sort ["mail", "music", "upgrade", "im", "backup", "finance"]











------------------------------------------------------------------------------
-- manage hooks
------------------------------------------------------------------------------


-- | A version of composeAll which returns a MaybeManageHook.
--
-- This is intended to be used within composeOne.
--
-- The reason to have something like this is as follows: imagine you have the
-- following composeOne:
--
--      composeOne
--          [ checkDock                 -?> doIgnore
--          , isDialog                  -?> doFloat
--          , className =? "Claws-mail" -?> doShift "mail"
--          , return True               -?> doF W.swapDown
--          ]
--
-- A composeOne is valuable here b/c we don't want to do the swapDown except in
-- the default case (as described in the XMonad faq). But what if we want to
-- combine the middle two hooks*? This is where this function comes in handy.
-- We extract the middle two hooks and run them through composeAllMaybe,
-- resulting in the following:
--
--      hooks =
--          [ (isDialog                 , doFloat)
--          , (className =? "Claws-mail", doShift "mail")
--          ]
--      composeOne
--          [ checkDock                 -?> doIgnore
--          , composeAllMaybe hooks
--          , return True               -?> doF W.swapDown
--          ]
--
--  Which has the following behavior:
--      The composeOne is like you'd expect, goes down one at a time until it
--      executes a hook. If our composeAllMaybe hook matches the window and
--      executes, the composeOne will stop there. If not, it continues on.
--
--      The composeAll maybe is just like a composeAll. It executes all that
--      match.
--
--
-- * While you may disagree with the wisdom of doing this, as it may hide the
-- dialogs for the mail client on a different workspace, it also ensures that
-- the dialogs will be there (and not on some random workspace.)
--          
composeAllMaybe :: [(Query Bool, ManageHook)] -> MaybeManageHook
composeAllMaybe l = (anyQuery l) -?> (composeAll $ makeDefiniteHooks l)

-- | Creates the list of ManageHooks
makeDefiniteHooks :: [(Query Bool, ManageHook)] -> [ManageHook]
makeDefiniteHooks = map makeDefiniteHook
    where
        makeDefiniteHook (q, z) = q --> z

-- | any query true
anyQuery :: [(Query Bool, ManageHook)] -> Query Bool
anyQuery l = foldl' (<||>) falseQuery querries
    where querries = map fst l


-- | constant query where bool is false
falseQuery :: Query Bool
falseQuery = liftX $ return False


-- | hooks that should be treated as a composeAll
-- Use xprop to find out this info
composeAllHooks = [
        -- floating
		  (isDialog                   , doCenterFloat)
		, (className =? "Xmessage"    , doCenterFloat)
		, (className =? "Gimp"        , doFloat)
		, (className =? "MPlayer"     , doFloat)

		-- shifting)
		, (className =? "Claws-mail"  , doShift "mail")
		, (className =? "Pidgin"      , doShift "im")
		, (className =? "SpiderOak"   , doShift "backup")
        , (className =? "Luckybackup" , doShift "backup")
        , (className =? "Gnucash"     , doShift "finance")
        , (className =? "Clementine"  , doShift "music")

		-- complex
		]

-- | My manageHook
myManageHook = composeOne
		[ checkDock     -?> doIgnore 		-- equivalent to manageDocks

        , composeAllMaybe composeAllHooks
		
		-- default
		, return True   -?> doF W.swapDown	-- new windows appear one down
		]









------------------------------------------------------------------------------
-- Log Hook related stuff
------------------------------------------------------------------------------
--
-- This allows us to have multiple dynamicLogWithPP's. The main reason to do
-- this is so we can have multiple xmobars (e.g: one up top and one at the
-- bottom)
--
-- Note: I have no idea whether this is broken/buggy in some subtle way, as my
-- Haskell (and particularly xmonad) experience is limited. That being said,
-- its currently working, so it can't be that bad.
--
-- My understanding is X () means that it effects the X monad but has no
-- return. Ie: something on the screen changes.
multiDynamicLogWithPP :: [PP] -> X ()
-- No PP, so don't do anything
multiDynamicLogWithPP [] = return ()
multiDynamicLogWithPP [pp] = dynamicLogWithPP pp
multiDynamicLogWithPP (pp:pps) = dynamicLogWithPP pp >> multiDynamicLogWithPP pps



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
    , ppUrgent = \w -> '*':w
    , ppSort = getSortByTag         -- show workspaces in lexicographical order
	}












------------------------------------------------------------------------------
--startup programs see: .xsession and .Xinitrc
--
-- I'm trying to avoid running programs on startup from my xmonad.hs, b/c then
-- whenever I reloaded xmonad (e.g: when testing out a configuration change),
-- it'd end up also starting up those programs. Which means I'd end up with
-- extra running copies, which I don't want.
------------------------------------------------------------------------------
--
-- Conky:
--
-- Conky is an edge case where if we let it remain from the previous xmonad
-- invocation, it ends up messing with xmobar. One solution is to use
-- 'own_window no' in the conkyrc file. But I have two conkys, which means that
-- doing this causes them to interferre with eachother. It also appears that
-- the 'own_window_type's desktop and panel have no effect in xmonad. Perhaps
-- xmonad doesn't recognize the attributes that conky sets (or I haven't
-- figured out how to make xmonad do so.)
--
-- So the easiest solution is to kill conky and restart it when doing a reload.
-- The script called does both.
--

myStartupHook :: X ()
myStartupHook = do
        spawn "~/.conky/conky_start.sh"







------------------------------------------------------------------------------
-- XMonad.Prompt configuration
------------------------------------------------------------------------------
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
	, position = Bottom		-- TODO: bottom vs top?
--	, alwaysHighlight = True
	}










------------------------------------------------------------------------------
-- Action prompt
------------------------------------------------------------------------------
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
actionPrompt config = inputPromptWithCompl config "Action" (mkComplFunFromList actionComplList) ?+ doAction










------------------------------------------------------------------------------
-- custom keybindings
------------------------------------------------------------------------------

-- | Helper function for cycling workspaces in lexicographical order
--  Uses findWorkspace to ensure workspaces are sorted lexographically, so
--  as to match the order they appear in xmobar. See README for more info.
cycleSorted :: Direction1D -> X ()
cycleSorted dir = findWorkspace getSortByTag dir NonEmptyWS 1 >>= (windows . W.greedyView)



-- | My keybindings
myKeys conf @(XConfig {XMonad.modMask = myModMask}) = M.fromList $
	-- program spawning
	[
	  ((myModMask, xK_p), shellPrompt myXPConfig)	-- like dmenu, but fits better with the rest of the theme
	, ((myModMask, xK_a), actionPrompt myXPConfig)	-- like wmii's action list (special comands to give the computer. E.g: shutdown, etc.)
	]
	++

	-- dynamic workspaces
	-- makes workspaces dynamic like in wmii and others, where they're
	--  created on use and deleted when no longer needed.
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
	--  Allows cycling through the workspaces in sorted lexicographical order.
	[
	  ((myModMask .|. shiftMask, xK_m), cycleSorted Next)
	, ((myModMask .|. shiftMask, xK_n), cycleSorted Prev)
	]
	++

	-- keybindings for audio
	[
	  ((0, xF86XK_AudioRaiseVolume), spawn "~/bin/raise_volume.sh")
	, ((0, xF86XK_AudioLowerVolume), spawn "~/bin/lower_volume.sh")
	, ((0, xF86XK_AudioMute       ), spawn "~/bin/mute_volume.sh")
	, ((0, xF86XK_AudioPlay       ), spawn "mpc toggle")
	, ((0, xF86XK_AudioStop       ), spawn "mpc stop")
	, ((0, xF86XK_AudioNext       ), spawn "mpc next")
	, ((0, xF86XK_AudioPrev       ), spawn "mpc prev")
	]
	++


	-- miscellaneous keybindings
	[
      ((mod4Mask,  xK_l     ), spawn "xscreensaver-command -lock")        -- note its mod4; TODO: switch workspaces?
    , ((myModMask, xK_i     ), spawn "~/bin/touchpad.py toggle")
    , ((0,         xK_Print ), spawn "~/bin/print_screen.sh")
    , ((0,         xK_Insert), pasteSelection)
    ]
    ++



	-- Example logging keybinding
--	[
--	  ((myModMask, xK_o), spawn ("echo \'" ++ show (XMonad.workspaces conf ) ++ "\' > /tmp/workspaces"))
--	]
--	++


	-- empty list at the end makes it easier to comment out keybinding
	-- blocks above. Because this way all blocks above have the (++) after
	-- them, and then commenting out the last block doesn't cause us to
	-- have to comment out a different block's (++)
	[]










------------------------------------------------------------------------------
-- Mouse bindings
------------------------------------------------------------------------------
myMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList
	[ ((modMask, 		     button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)	-- float and move/drag
	, ((modMask, 		     button2), windows . (W.shiftMaster .) . W.focusWindow)			-- raise to top of stack
	, ((modMask, 		     button3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster) 	-- float and resize
	, ((myModMask .|. shiftMask, button1), \_ -> withFocused $ windows . W.sink) 				-- Push window back into tiling
	]









------------------------------------------------------------------------------
-- main
------------------------------------------------------------------------------
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

	xmonad  $ withUrgencyHook NoUrgencyHook
            $ defaultConfig
		{
                  terminal    	= myTerminal
		, modMask     	= myModMask
		, keys 	      	= myKeys <+> keys defaultConfig
		, mouseBindings = myMouseBindings

		--
		--- aesthetics
		--
		, borderWidth 	     = myBorderWidth
		, normalBorderColor  = myNormalBorderColor
		, focusedBorderColor = myFocusedBorderColor

		--
		--- layouts and window management
		--
		, manageHook      = myManageHook <+> manageDocks <+> manageHook defaultConfig
		, layoutHook      = myLayoutHook
		, workspaces      = myWorkspaces
		, handleEventHook = docksEventHook <+> handleEventHook defaultConfig

        --
        --- other hooks
        --
        , startupHook = myStartupHook

		--
		---  xmobars
		--   see above for def of xmobarXPP's
		--
		, logHook = multiDynamicLogWithPP
				[ xmobarTopPP xmobarTopProc
				, xmobarBottomPP xmobarBottomProc
				]
		}



