import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.SpawnOn
import XMonad.Config.Xfce
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Hooks.Place
import XMonad.Util.Run
import XMonad.Util.NamedScratchpad
import XMonad.Util.Scratchpad
import XMonad.Util.SpawnOnce
import System.Exit
import System.IO
import qualified Data.Map as M
import qualified XMonad.StackSet as W

--Moves the focused window to the end of the stack
--I prefer this behaviour but you might not
--don't really need this anymore since I found out about
--InsertPosition End Newer in ManageHook
moveEnd :: W.StackSet i l a s sd -> W.StackSet i l a s sd
moveEnd = W.modify' moveEnd'

moveEnd' :: W.Stack a -> W.Stack a
moveEnd' (W.Stack t ls rs) = W.Stack t ((reverse rs) ++ ls) []

--Makes workspaces in xmobar clickable, copied from:
--http://www.arch-ed.dk/xmobar-clickable-workspaces/
xmobarEscape = concatMap doubleLts
  where doubleLts '<' = "<<"
        doubleLts x   = [x]
 
myWorkspaces :: [String]        
myWorkspaces = clickable . (map xmobarEscape) $ [" 一 "," 二 "," 三 "," 四 "]
                                                                              
  where                                                                      
         clickable l = [ "<action=xdotool key super+" ++ show (n) ++ ">" ++ ws ++ "</action>" |
                             (i,ws) <- zip [1..4] l,                                        
                            let n = i ]

--Scratchpads for ranger and mocp, 2 common terminal programs I use
scratchpads = 
  [ NS "ranger" "urxvt -name ranger -e ranger" (resource =? "ranger") myPosition
  , NS "mocp" "urxvt -name mocp -e mocp" (resource =? "mocp") myPosition
  ] where myPosition = customFloating $ W.RationalRect 0.2 0.2 0.6 0.6

--Keybinds done from scratch because I don't like accidentally
--pressing something and having no idea what I just did
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList
  [ ((modm, xK_s), shiftToNext >> nextWS >> windows moveEnd) --shift focused window to next ws and switch
  , ((modm, xK_a), shiftToPrev >> prevWS >> windows moveEnd) --shift focused window to prev ws and switch
  , ((modm, xK_1), windows $ W.greedyView $ myWorkspaces !! 0) --focus workspace 1-4
  , ((modm, xK_2), windows $ W.greedyView $ myWorkspaces !! 1)
  , ((modm, xK_3), windows $ W.greedyView $ myWorkspaces !! 2)
  , ((modm, xK_4), windows $ W.greedyView $ myWorkspaces !! 3)
  , ((modm, xK_w), kill) --close a window
  , ((modm, xK_space), spawn "dmenu_run") --run dmenu
  , ((modm, xK_Tab), sendMessage NextLayout) --switch layouts
  , ((modm, xK_h), sendMessage Shrink) --shrink master and slave sizes (fuck you django devs)
  , ((modm, xK_l), sendMessage Expand)
  , ((modm, xK_f), sendMessage ToggleStruts) --toggle fullscreen mode/allow xmobar to be covered
  , ((modm, xK_j), windows W.focusDown) --switches focus of windows on the current workspace
  , ((modm, xK_k), windows W.focusUp)
  , ((modm .|. shiftMask, xK_j), windows W.swapDown) --swap focused window on the current workspace
  , ((modm .|. shiftMask, xK_k), windows W.swapUp)
  , ((modm, xK_t), withFocused $ windows . W.sink) --sink window to tiling
  , ((modm, xK_Print), spawn "~/.xmonad/scripts/upload -f") --take fullscreen screenshot and upload it
  , ((modm .|. shiftMask, xK_Print), spawn "~/.xmonad/scripts/upload -s") --select an area to screenshot and upload
  , ((modm, xK_r), spawn "~/.xmonad/scripts/upload -c") --upload last modified item in ~/screenshots, useful for mpv
  , ((modm, xK_p), spawn "mocp -G") --pause-play/prev/next song, used with mocp
  , ((modm, xK_i), spawn "mocp -r")
  , ((modm, xK_o), spawn "mocp -f")
  , ((modm, xK_g), spawn "firefox") --launches firefox
  , ((modm, xK_u), spawn "thunar") --launches thunar
  , ((modm, xK_8), namedScratchpadAction scratchpads "ranger") --opens scratchpads
  , ((modm, xK_9), namedScratchpadAction scratchpads "mocp")
  , ((modm, xK_0), scratchpadSpawnActionTerminal "urxvt")
  , ((0, 0x1008FF12), spawn "amixer -D pulse set Master 1+ togglemute") --mess with the volume controls
  , ((0, 0x1008FF11), spawn "amixer -q set Master 3%-")
  , ((0, 0x1008FF13), spawn "amixer -q set Master 3%+")
  , ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf) --open a terminal
  , ((modm .|. shiftMask, xK_q), io (exitWith ExitSuccess)) --log out
  , ((modm, xK_q), spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi") 
  , ((modm, xK_F7), spawn "~/.xmonad/scripts/screenoff") --recompile/reload
  ]


main = do
  spawn "compton" --compositing manager
  spawn "dunst" --notification daemon
  spawn "feh --bg-scale ~/.xmonad/background.png" --draw wallpaper
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc" --spawn xmobar
  xmonad $ defaultConfig
    { terminal = "urxvt"
    , keys = myKeys
    , workspaces = myWorkspaces
    , modMask = mod4Mask
    , borderWidth = 0
    , layoutHook = smartSpacing 1 $ avoidStruts $ smartBorders $ layoutHook defaultConfig
    , logHook = dynamicLogWithPP xmobarPP --custom xmobar ricing stuff
      { ppOutput = hPutStrLn xmproc
      , ppSort = fmap (.scratchpadFilterOutWorkspace) $ ppSort defaultPP
      , ppHidden = xmobarColor  "#90626d" ""
      , ppCurrent = xmobarColor "#ede7e0" ""
      , ppHiddenNoWindows = id 
      , ppLayout = const ""
      , ppSep = ": "
      , ppTitle = shorten 110
      }
    , manageHook = composeAll
      [ insertPosition End Newer 
      , manageDocks
      , namedScratchpadManageHook scratchpads 
      , scratchpadManageHook (W.RationalRect 0.2 0.2 0.6 0.6)
      , manageHook defaultConfig
      ]
    }
