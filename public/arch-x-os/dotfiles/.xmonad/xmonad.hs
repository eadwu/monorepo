import XMonad

import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders

import XMonad.Util.Run
import XMonad.Util.EZConfig(additionalKeys)

import Data.Monoid
import System.Exit

import qualified Data.Map as M
import qualified XMonad.StackSet as W

{- Key Bindings  -}
keybinds conf@ XConfig {XMonad.modMask = modm} = M.fromList $
  -- launch rofi
  [((modm, xK_z), spawn "rofi -show run -lines 3 -bw 1 -width 100 -padding 50%"),
  -- launch urxvt
  ((modm, xK_Return), spawn $ XMonad.terminal conf),
  -- close focused window
  ((modm .|. shiftMask, xK_q), kill),
  -- quit xmonad
  ((modm .|. shiftMask, xK_e), io (exitWith ExitSuccess)),
  -- restart xmonad
  ((modm .|. shiftMask, xK_r), spawn "xmonad --recompile; xmonad --restart")
  ]
  ++
  -- mod-[1..4], switch to workspace N
  -- mod-shift-[1..4], move client to workspace N
  [((m .|. modm, k), windows $ f i) |
    (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_4],
    (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ]

{- Mouse Bindings -}
mousebinds XConfig {XMonad.modMask = modm} = M.fromList
  -- mod-button1, Set the window to floating mode and move by dragging
  [((modm, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster),
  -- mod-button2, Raise the window to the top of the stack
  ((modm, button2), \w -> focus w >> windows W.shiftMaster)
  ]

{- Layout -}
layout = smartBorders $ tiled ||| Mirror tiled ||| Full
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled = smartSpacing 5 $ Tall nmaster delta ratio
    -- The default number of windows in the master pane
    nmaster = 1
    -- Default proportion of screen occupied by master pane
    ratio = 1 / 2
    -- Percent of screen to increment by when resizing panes
    delta = 3 / 100  

{- Window Rules -}
rules = composeAll
  [className =? "Opera developer" --> doShift "web",
  className =? "Atom" --> doShift "code",
  className =? "discord" --> doShift "social",

  className =? "Nitrogen" --> doFloat,
  className =? "Pinentry" --> doFloat
  ] 

main = do
  -- spawnPipe "/usr/bin/redshift -l 40.7:73.8"
  spawnPipe "pa-applet"
  spawnPipe "/usr/bin/nitrogen --restore"
  spawnPipe "xsetroot -cursor_name left_ptr"
  spawnPipe "~/.config/conky/init-conky"
  spawnPipe "/usr/bin/compton -b -f --config ~/.config/compton.conf"
  xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmonad/xmobarrc"
  xmonad $ docks def {
    terminal = "urxvt",
    focusFollowsMouse = True, -- focus on window on hover
    clickJustFocuses = False, -- focus on window on click also
    borderWidth = 1,
    modMask = mod1Mask, -- left alt; for right alt mod3Mask
    workspaces = [ "1:WEB", "2:CODE", "3:SOCIAL", "4:OTHER" ],
    normalBorderColor = "#333333", -- unfocused border color for windows
    focusedBorderColor = "#1793D1", -- focused border color for windows

    keys = keybinds,
    mouseBindings = mousebinds,
    
    layoutHook = avoidStrutsOn [U] $ layout,
    manageHook = manageDocks <+> rules <+> manageHook def,
    handleEventHook = mempty,
    logHook = dynamicLogWithPP $ xmobarPP {
      ppSep = " => ",
      ppTitle = xmobarColor "D3869B" "" . shorten 50,
      ppOrder = \(ws:_:t:_) -> [ws,t],
      ppOutput = hPutStrLn xmproc,
      ppCurrent = xmobarColor "#83A598" "" . wrap "[" "]"
    },
    startupHook = setWMName "LG3D"
  } `additionalKeys`
    [("<XF86PowerOff>", spawn "oblogout_blur"),
    ("<XF86AudioLowerVolume>", spawn "sh -c \"pactl set-sink-mute 0 false ; pactl set-sink-volume 0 -5%\""),
    ("<XF86AudioMute>", spawn "pactl set-sink-mute 0 toggle"),
    ("<XF86AudioRaiseVolume>", spawn "sh -c \"pactl set-sink-mute 0 false ; pactl set-sink-volume 0 +5%\"")
    {- -- /sys/class/backlight/{folder}/{max_brightness} > brightness 
    ("<XF86MonBrightnessDown>", spawn ""),
    ("<XF86MonBrightnessUp>", spawn ""),
    -- /sys/class/leds/smc::kbd_backlight
    ("<XF86KbdBrightnessDown>", spawn ""),
    ("<XF86KbdBrightnessUp>", spawn "") -}
    ]
