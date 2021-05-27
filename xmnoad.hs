import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import Graphics.X11.ExtraTypes.XF86
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import System.IO.Unsafe
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
import XMonad.Layout.Tabbed
import Data.Maybe
import Data.Bifunctor
import Data.List as DL
import Data.Char as DC
import XMonad.Util.Run

myKeys = [
  ((0, xF86XK_AudioMute), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle"),
  ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ -2%"),
  ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ +2%"),
  ((0, xF86XK_MonBrightnessUp), spawn "light -A 3"),
  ((0, xF86XK_MonBrightnessDown), spawn "light -U 3"),
  ((0, xF86XK_MonBrightnessUp), spawn "lux -a 3%"),
  ((0, xF86XK_MonBrightnessDown), spawn "lux -s 3%"),
  ((0, xK_Print), spawn "scrot ~/Pictures/screen_%Y-%m-%d-%H-%M-%S.png"), -- take a screenshot
  ((mod4Mask .|. shiftMask, xK_z), spawn myScreenLocker),
  ((mod4Mask .|. shiftMask, xK_l), spawn mySuspend),
  ((mod4Mask .|. shiftMask, xK_f), spawn myFileBrowser),
  ((mod4Mask .|. controlMask, xK_Right), nextWS), -- next workspace
  ((mod4Mask .|. shiftMask, xK_Right), shiftToNext), --move to next
  ((mod4Mask .|. controlMask, xK_Left), prevWS),
  ((mod4Mask .|. shiftMask, xK_Left), shiftToPrev),
  ((mod4Mask, xK_p), spawn myLauncher),
  ((mod4Mask .|. shiftMask, xK_p), spawn myLauncherDesktop),
  ((mod4Mask, xK_s), spawn mySSH),
  ((mod4Mask, xK_n), spawn myEmacsCapture),
  ((mod4Mask, xK_c ), spawn myEmacsFrame),
  ((mod4Mask, xK_g), spawn "rofi -show window -show-icons"),
  ((mod4Mask .|. shiftMask, xK_g), spawnSelected defaultGSConfig ["i3lock -c 000000", "i3lock -c 000000 && systemctl suspend"]),
  ((mod4Mask, xK_f), sendMessage ToggleStruts) -- Fullscreen.
  ]

myTerminal = "urxvt -e tmux"
myTouchpad = "~/Workspace/scripts/touchpad_xinput.sh"
myWallpaper = "wal -i ~/Pictures/wallpapers" -- Random wallpaper
myScreenLocker = "i3lock -c 000000"
mySuspend = "i3lock -c 000000 && systemctl suspend"
myFileBrowser = "urxvt -e nnn -de"
myLauncher = "rofi -show run"
myLauncherDesktop = "rofi -show drun -show-icons"
mySSH = "rofi -show ssh"
myEmacsCapture = "emacsclient -e \"(make-capture-frame)\""
myEmacsFrame = "emacsclient --create-frame"

myStartup :: X ()
myStartup = do
  setWMName "LG3D" -- Because Java (http://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Hooks-SetWMName.html)
  spawn myWallpaper
  spawn myTouchpad -- Set my touchpad settings
  spawn "exec xautolock -detectsleep -time 30 -locker 'i3lock -c 000000'" -- Lock screen after 5min

myTabConfig = def { activeColor = "#556064",
                    inactiveColor = "#2F3D44",
                    urgentColor = "#FDF6E3",
                    activeBorderColor = "#454948",
                    inactiveBorderColor = "#454948",
                    urgentBorderColor = "#268BD2",
                    activeTextColor = "#80FFF9",
                    inactiveTextColor = "#1ABC9C",
                    urgentTextColor = "#1ABC9C"
                  }

myLayout = avoidStruts $ toggleLayouts(noBorders Full) $ smartBorders $ noBorders Full ||| tiled ||| Mirror tiled ||| Grid ||| tabbed shrinkText myTabConfig
  where
    tiled   = smartSpacing 5 $ Tall nmaster delta ratio
    nmaster = 1
    ratio   = 1/2
    delta   = 3/100

myWorkspaces = ["1","2","3","4","5","6","7","8","9"]

-- A function to get data from .Xresources
getFromXres :: String -> IO String
getFromXres key = fromMaybe "" . findValue key <$> runProcessWithInput "xrdb" ["-query"] ""
  where
    findValue :: String -> String -> Maybe String
    findValue xresKey xres =
      snd <$> (
                DL.find ((== xresKey) . fst)
                $ catMaybes
                $ splitAtColon
                <$> lines xres
              )

    splitAtColon :: String -> Maybe (String, String)
    splitAtColon str = splitAtTrimming str <$> (DL.elemIndex ':' str)

    splitAtTrimming :: String -> Int -> (String, String)
    splitAtTrimming str idx = bimap trim trim . (second tail) $ splitAt idx str

    trim :: String -> String
    trim = DL.dropWhileEnd (DC.isSpace) . DL.dropWhile (DC.isSpace)

fromXres :: String -> String
fromXres = unsafePerformIO . getFromXres

main = do
  xmproc <- spawnPipe "~/Workspace/scripts/run_polybar.sh" -- Used to reload polybar
  xmonad $ ewmh azertyConfig
    {
      manageHook  = manageDocks <+> manageHook azertyConfig,
      terminal    = myTerminal,
      modMask     = mod4Mask,
      borderWidth = 2,
      normalBorderColor  = "#303030",
      focusedBorderColor = fromXres "*.color7",
      workspaces  = myWorkspaces,
      startupHook = myStartup,
      -- To use with polybar:
      logHook = ewmhDesktopsLogHook,
      layoutHook  = myLayout,
      handleEventHook = docksEventHook
    } `additionalKeys` myKeys
