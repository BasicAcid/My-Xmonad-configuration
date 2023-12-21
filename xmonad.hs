import Data.Bifunctor
import Data.Char as DC
import Data.List as DL
import Data.Maybe
import Graphics.X11.ExtraTypes.XF86
import System.IO.Unsafe
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Config.Azerty
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.Decoration
import XMonad.Layout.Grid
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run
import XMonad.Util.Themes
--import XMonad.Hooks.WindowSwallowing -- Require newer ghc version

myKeys :: [((KeyMask, KeySym), X ())]
myKeys =
  [ ((0, xF86XK_AudioMute), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
  , ( (0, xF86XK_AudioLowerVolume)
    , spawn "pactl set-sink-volume @DEFAULT_SINK@ -2%")
  , ( (0, xF86XK_AudioRaiseVolume)
    , spawn "pactl set-sink-volume @DEFAULT_SINK@ +2%")
  , ((0, xF86XK_MonBrightnessUp), spawn "light -A 3")
  , ((0, xF86XK_MonBrightnessDown), spawn "light -U 3")
  , ((0, xF86XK_MonBrightnessUp), spawn "lux -a 3%")
  , ((0, xF86XK_MonBrightnessDown), spawn "lux -s 3%")
  , ((0, xK_Print), spawn "scrot ~/Pictures/screen_%Y-%m-%d-%H-%M-%S.png") -- take a screenshot
  , ((mod4Mask .|. shiftMask, xK_z), spawn screenLockerCmd)
  , ((mod4Mask .|. shiftMask, xK_l), spawn suspendCmd)
  , ((mod4Mask .|. shiftMask, xK_f), spawn fileBrowserCmd)
  , ((mod4Mask .|. controlMask, xK_Right), nextWS) -- next workspace
  , ((mod4Mask .|. shiftMask, xK_Right), shiftToNext) --move to next
  , ((mod4Mask .|. controlMask, xK_Left), prevWS)
  , ((mod4Mask .|. shiftMask, xK_Left), shiftToPrev)
  , ((mod4Mask, xK_p), spawn launcherCmd)
  , ((mod4Mask .|. shiftMask, xK_p), spawn launcherDesktopCmd)
  , ((mod4Mask, xK_s), spawn sshCmd)
  , ((mod4Mask, xK_n), spawn emacsCaptureCmd) -- Require noflet Emacs package.
  , ((mod4Mask, xK_c), spawn emacsFrameCmd)
  , ((mod4Mask, xK_g), spawn "rofi -show window -show-icons")
  --Not really used (spawn a logout menu), commented because defaultGSConfig is deprecated, could not find a solution.
  --((mod4Mask .|. shiftMask, xK_g), spawnSelected defaultGSConfig ["i3lock -c 000000", "i3lock -c 000000 && systemctl suspend"]),
  , ((mod4Mask, xK_f), sendMessage ToggleStruts) -- Fullscreen.
  ]

terminalCmd :: String
terminalCmd = "kitty"

touchpadCmd :: String
touchpadCmd = "~/Workspace/scripts/touchpad_xinput.sh" -- use touchpad_synclient.sh on Debian

wallpaperCmd :: String
wallpaperCmd = "wal -i ~/Pictures/wallpapers" -- Random wallpaper

screenLockerCmd :: String
screenLockerCmd = "i3lock -c 000000"

suspendCmd :: String
suspendCmd = "i3lock -c 000000 && systemctl suspend"

fileBrowserCmd :: String
fileBrowserCmd = "kitty -e nnn -e"

launcherCmd :: String
launcherCmd = "rofi -show run"

launcherDesktopCmd :: String
launcherDesktopCmd = "rofi -show drun -show-icons"

sshCmd :: String
sshCmd = "rofi -show ssh"

emacsCaptureCmd :: String
emacsCaptureCmd = "emacsclient -e \"(make-capture-frame)\""

emacsFrameCmd :: String
emacsFrameCmd = "emacsclient --create-frame"

myStartup :: X ()
myStartup = do
  setWMName "LG3D" -- Because Java (http://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Hooks-SetWMName.html).
  spawn wallpaperCmd
  spawn touchpadCmd
  spawn "exec xautolock -detectsleep -time 30 -locker 'i3lock -c 000000'" -- Lock screen after 5min
  spawn "xcompmgr -c &" -- Compositor (will not spawn more than 1 instances).

-- https://hackage.haskell.org/package/xmonad-contrib-0.17.0/docs/XMonad-Layout-Decoration.html#t:Theme
-- themeConfig :: Theme
-- themeConfig =
--   def
--     { activeColor = "#556064"
--     , inactiveColor = "#2F3D44"
--     , urgentColor = "#FDF6E3"
--     , activeBorderColor = "#454948"
--     , inactiveBorderColor = "#454948"
--     , urgentBorderColor = "#268BD2"
--     , activeTextColor = "#80FFF9"
--     , inactiveTextColor = "#1ABC9C"
--     , urgentTextColor = "#1ABC9C"
--     }

tabbedConfig = def { inactiveBorderColor = "#FF0000"
                  , activeTextColor = "#00FF00"}

tiled :: ModifiedLayout Spacing Tall a
tiled = smartSpacing 5 $ Tall nmaster delta ratio

nmaster :: Int
nmaster = 1

ratio :: Rational
ratio = 1 / 2

delta :: Rational
delta = 3 / 100

-- Well, that's a type signature.
myLayout :: XMonad.Layout.LayoutModifier.ModifiedLayout AvoidStruts (ToggleLayouts (XMonad.Layout.LayoutModifier.ModifiedLayout WithBorder Full) (XMonad.Layout.LayoutModifier.ModifiedLayout SmartBorder (Choose (XMonad.Layout.LayoutModifier.ModifiedLayout WithBorder Full) (Choose (XMonad.Layout.LayoutModifier.ModifiedLayout Spacing Tall) (Choose (Mirror (XMonad.Layout.LayoutModifier.ModifiedLayout Spacing Tall)) (Choose Grid (XMonad.Layout.LayoutModifier.ModifiedLayout (XMonad.Layout.Decoration.Decoration TabbedDecoration XMonad.Layout.Decoration.DefaultShrinker) XMonad.Layout.Simplest.Simplest))))))) Window
myLayout =
  avoidStruts $
  toggleLayouts (noBorders Full) $
  smartBorders $
  noBorders Full |||
  tiled ||| Mirror tiled ||| Grid ||| tabbed shrinkText tabbedConfig -- https://hackage.haskell.org/package/xmonad-contrib-0.17.0/docs/XMonad-Util-Themes.html

myWorkspaces :: [String]
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

findValue :: String -> String -> Maybe String
findValue xresKey xres =
  snd <$> DL.find ((== xresKey) . fst) (catMaybes $ splitAtColon <$> lines xres)

splitAtColon :: String -> Maybe (String, String)
splitAtColon str = splitAtTrimming str <$> DL.elemIndex ':' str

splitAtTrimming :: String -> Int -> (String, String)
splitAtTrimming str idx = bimap trim (trim . tail) $ splitAt idx str

trim :: String -> String
trim = DL.dropWhileEnd DC.isSpace . DL.dropWhile DC.isSpace

getFromXres :: String -> IO String
getFromXres key =
  fromMaybe "" . findValue key <$> runProcessWithInput "xrdb" ["-query"] ""

-- Need to find a way to remove unsafePerformIO.
fromXres :: String -> String
fromXres = unsafePerformIO . getFromXres

main :: IO ()
main = do
  _ <- spawnPipe "~/Workspace/scripts/run_polybar.sh" -- Used to reload polybar
  xmonad $
    ewmh $
    docks
      azertyConfig
        { manageHook = manageDocks <+> manageHook azertyConfig
        , terminal = terminalCmd
        , modMask = mod4Mask
        , borderWidth = 2
        , normalBorderColor = "#303030"
        , focusedBorderColor = fromXres "*.color7"
        , workspaces = myWorkspaces
        , startupHook = myStartup
      -- To use with polybar:
      --logHook = ewmhDesktopsLogHook, --old conf, not necesary since ewmh is called up there. To remove.
        , layoutHook = myLayout
      --handleEventHook = docksEventHook --old conf, not necesary since docks is called up there. To remove.
      --, handleEventHook = swallowEventHook (className =? "Alacritty" <||> className =? "Kitty") (return True)
        } `additionalKeys`
    myKeys
