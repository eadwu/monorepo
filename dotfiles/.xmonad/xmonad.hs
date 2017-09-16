import XMonad

import XMonad.Hooks.SetWMName -- setWMName
import XMonad.Hooks.DynamicLog -- dynamicLogWithPP, ppSep, ppTitle, ppOrder, ppOutput, ppCurrent
import XMonad.Hooks.ManageDocks -- manageDocks, avoidStruts

import XMonad.Layout.Spacing -- smartSpacing
import XMonad.Layout.Maximize -- maximizeWithPadding, maximizeRestore
import XMonad.Layout.Minimize -- minimize, minimizeWindow, MinimizeMsg(RestoreNextMinimizedWin)
import XMonad.Layout.NoBorders -- smartBorders

import XMonad.Util.Run -- hPutStrLn, spawnPipe, safeSpawnProg
import XMonad.Util.EZConfig -- additionalKeys

import Data.Monoid -- mempty
import System.Exit

import qualified Data.Map as M
import qualified XMonad.StackSet as W

{- Key Bindings  -}
keybinds conf@ XConfig {XMonad.modMask = modm} = M.fromList $
  -- launch rofi
  [((modm, xK_z), spawn "rofi -show run"),
  -- launch urxvt
  ((modm, xK_Return), safeSpawnProg $ XMonad.terminal conf),
  -- rorate layout(s)
  ((modm, xK_space), do
    sendMessage $ ToggleStruts
    sendMessage $ NextLayout),
  -- default layout
  ((modm .|. shiftMask, xK_space), do
    sendMessage $ SetStruts [U] []
    setLayout $ XMonad.layoutHook conf),
  -- maximize focused window
  ((modm, xK_f), do
    sendMessage $ ToggleStruts
    withFocused $ sendMessage . maximizeRestore),
  -- minimize focused window
  ((modm .|. shiftMask, xK_f), withFocused $ minimizeWindow),
  -- restore windows
  ((modm .|. controlMask, xK_f), do
    sendMessage $ SetStruts [U] []
    sendMessage $ RestoreNextMinimizedWin),
  -- close focused window
  ((modm .|. shiftMask, xK_q), kill),
  -- quit xmonad
  ((modm .|. shiftMask, xK_e), io $ exitWith ExitSuccess),
  -- restart xmonad
  ((modm .|. shiftMask, xK_r), spawn "killall conky redshift; xmonad --recompile; xmonad --restart")]
  ++
  -- mod-[1..4], switch to workspace N
  -- mod-shift-[1..4], move client to workspace N
  [((m .|. modm, k), windows $ f i) |
    (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_4],
    (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

{- Mouse Bindings -}
mousebinds XConfig {XMonad.modMask = modm} = M.fromList
  -- mod-button1, Set the window to floating mode and move by dragging
  [((modm, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster),
  -- mod-button2, Raise the window to the top of the stack
  ((modm, button2), \w -> focus w >> windows W.shiftMaster)]

{- Layout -}
layout = smartBorders . maximizeWithPadding 0 . minimize $ tiled ||| Full
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
  [manageDocks,

  title =? "Task Manager - Opera" --> doFloat,
  title =? "File Operation Progress" --> doFloat,
  title =? "Download Chrome Extension" --> doFloat,

  className =? "Opera developer" --> doShift "1:WEB",
  className =? "Atom" --> doShift "2:CODE",
  className =? "discord" --> doShift "3:SOCIAL",

  className =? "Nitrogen" --> doFloat,
  className =? "Pinentry" --> doFloat,
  className =? "Oblogout" --> doFloat,
  className =? "Xfce4-screenshooter" --> doFloat,
  className =? "Lightdm-gtk-greeter-settings" --> doFloat]

main = do
  mapM_ spawn
    ["~/.config/conky/init-conky",
    "xsetroot -cursor_name left_ptr",
    "/usr/bin/env nitrogen --restore",
    "/usr/bin/env redshift -l 40.7:73.8",
    "/usr/bin/env compton -b -f --config ~/.config/compton.conf"]
  xmproc <- spawnPipe "/usr/bin/env xmobar ~/.xmonad/xmobarrc"
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

    layoutHook = avoidStrutsOn [U] layout,
    manageHook = rules <+> manageHook def,
    handleEventHook = mempty,
    logHook = dynamicLogWithPP $ xmobarPP {
      ppSep = "  ->  ",
      ppTitle = xmobarColor "D3869B" "" . shorten 42,
      ppOrder = \(ws:_:t:_) -> [ws,t],
      ppOutput = hPutStrLn xmproc,
      ppCurrent = xmobarColor "#83A598" "" . wrap "[" "]"
    },
    startupHook = setWMName "LG3D"
  } `additionalKeys`
    -- XF86PowerOff
    [((0, 0x1008FF2A), spawn "oblogout_blur"),
    -- XF86AudioLowerVolume
    ((0, 0x1008FF11), spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%"),
    ((mod1Mask, 0x1008FF11), spawn "pactl set-sink-volume @DEFAULT_SINK@ -1%"),
    -- XF86AudioMute
    ((0, 0x1008FF12), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle"),
    -- XF86AudioRaiseVolume
    ((0, 0x1008FF13), spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%"),
    ((mod1Mask, 0x1008FF13), spawn "pactl set-sink-volume @DEFAULT_SINK@ +1%"),
    -- XF86MonBrightnessDown
    ((0, 0x1008FF03), spawn "sudo ~/.xmonad/bin/mon_backlight -"),
    ((mod1Mask, 0x1008FF03), spawn "sudo ~/.xmonad/bin/mon_backlight - 5"),
    -- XF86MonBrightnessUp
    ((0, 0x1008FF02), spawn "sudo ~/.xmonad/bin/mon_backlight +"),
    ((mod1Mask, 0x1008FF02), spawn "sudo ~/.xmonad/bin/mon_backlight + 5"),
    -- XF86KbdBrightnessDown
    ((0, 0x1008FF06), spawn "sudo ~/.xmonad/bin/kbd_backlight -"),
    ((mod1Mask, 0x1008FF06), spawn "sudo ~/.xmonad/bin/kbd_backlight - 5"),
    -- XF86KbdBrightnessUp
    ((0, 0x1008FF05), spawn "sudo ~/.xmonad/bin/kbd_backlight +"),
    ((mod1Mask, 0x1008FF05), spawn "sudo ~/.xmonad/bin/kbd_backlight + 5")]
