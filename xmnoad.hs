import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import Graphics.X11.ExtraTypes.XF86
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import XMonad.Config.Azerty
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.NoBorders
import XMonad.Layout.Grid
import XMonad.Actions.WindowGo
import XMonad.Actions.GridSelect
import XMonad.Layout.ToggleLayouts
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe, safeSpawn)
import Data.List (sortBy)
import Data.Function (on)
import Control.Monad (forM_, join)
import XMonad.Util.NamedWindows (getName)
import qualified XMonad.StackSet as W
import XMonad.Hooks.SetWMName
import XMonad.Actions.CycleWS
import XMonad.Hooks.EwmhDesktops

myKeys = [
  ((0, xF86XK_AudioMute), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle"),
  ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ -2%"),
  ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ +2%"),
  ((0, xF86XK_MonBrightnessUp), spawn "light -A 3"),
  ((0, xF86XK_MonBrightnessDown), spawn "light -U 3"),
  ((0, xK_Print), spawn "scrot ~/Pictures/screen_%Y-%m-%d-%H-%M-%S.png"), -- take a screenshot
  ((mod4Mask .|. shiftMask, xK_z), spawn myScreenLocker),
  ((mod4Mask .|. shiftMask, xK_l), spawn mySuspend),
  ((mod4Mask .|. shiftMask, xK_f), spawn myFileBrowser),
  ((mod4Mask .|. controlMask, xK_Right), nextWS), -- next workspace
  ((mod4Mask .|. shiftMask, xK_Right), shiftToNext), --move to next
  ((mod4Mask .|. controlMask, xK_Left), prevWS),
  ((mod4Mask .|. shiftMask, xK_Left), shiftToPrev),
  ((mod4Mask, xK_p), spawn myLauncher),
  ((mod4Mask, xK_s), spawn mySSH),
  ((mod4Mask, xK_g), goToSelected defaultGSConfig),
  ((mod4Mask .|. shiftMask, xK_g), spawnSelected defaultGSConfig ["i3lock -c 000000", "i3lock -c 000000 && systemctl suspend"]),
  ((mod4Mask, xK_f), sendMessage ToggleStruts)
  ]

myTerminal = "urxvt"
myTouchpad = "~/.xmonad/touchpad.sh"
myWallpaper = "feh --randomize --bg-scale ~/Pictures/wallpapers/*" -- Random wallpaper
myScreenLocker = "i3lock -c 000000"
mySuspend = "i3lock -c 000000 && systemctl suspend"
myFileBrowser = "urxvt -e ranger"
myLauncher = "rofi -show run"
mySSH = "rofi -show ssh"

myStartup :: X ()
myStartup = do
  setWMName "LG3D" -- Because Java (http://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Hooks-SetWMName.html)
  spawn myWallpaper
  spawn myTouchpad -- Set my touchpad settings
  spawn "exec xautolock -detectsleep -time 30 -locker 'i3lock -c 000000'" -- Lock screen after 5min
  spawn "exec redshift -l 46:2 -m randr"

myLayout = avoidStruts $ toggleLayouts(noBorders Full) $ smartBorders $ noBorders Full ||| tiled ||| Mirror tiled ||| Grid
  where
    tiled   = smartSpacing 5 $ Tall nmaster delta ratio
    nmaster = 1
    ratio   = 1/2
    delta   = 3/100

myWorkspaces = ["1","2","3","4","5","6","7","8","9"]

main = do
  xmproc <- spawnPipe "/usr/bin/xmobar /home/david/.xmonad/xmobarrc"
  xmonad $ azertyConfig
    {
      manageHook  = manageDocks <+> manageHook azertyConfig,
      terminal    = myTerminal,
      modMask     = mod4Mask,
      borderWidth = 2,
      normalBorderColor  = "#303030",
      focusedBorderColor = "#90ee90",
      workspaces  = myWorkspaces,
      startupHook = myStartup,
      logHook = dynamicLogWithPP xmobarPP
                { ppOutput = hPutStrLn xmproc,
                  ppCurrent = xmobarColor "#1ABC9C" "" . wrap "[" "]",
                  ppTitle = xmobarColor "#1ABC9C" "" . shorten 0,
                  ppUrgent  = xmobarColor "red" "yellow"
                },
      layoutHook  = myLayout,
      handleEventHook = docksEventHook
    } `additionalKeys` myKeys
