{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Data.Monoid
import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8
import Graphics.X11.ExtraTypes.XF86
import XMonad
import XMonad.Actions.Volume
import XMonad.Actions.SpawnOn
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Grid
import XMonad.Layout.MultiColumns
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import qualified XMonad.StackSet as W -- to shift and float windows
import XMonad.Util.Dzen
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

alert :: forall a. RealFrac a => a -> X ()
alert = dzenConfig centered . show . (round :: a -> Int)

centered :: (Int, [String]) -> X (Int, [String])
centered =
        onCurr (center 150 66)
    >=> font "-*-helvetica-*-r-*-*-64-*-*-*-*-*-*-*"
    >=> addArgs ["-fg", "#80c0ff"]
    >=> addArgs ["-bg", "#000040"]

toggleMute' :: MonadIO m => m Double
toggleMute' = do
    res <- toggleMute
    case res of
        False -> getVolume
        True -> return 0

prettyPrinter :: D.Client -> PP
prettyPrinter dbus = defaultPP
    { ppOutput   = dbusOutput dbus
    , ppTitle    = pangoSanitize
    , ppCurrent  = pangoColor "green" . wrap "[" "]" . pangoSanitize
    , ppVisible  = pangoColor "yellow" . wrap "(" ")" . pangoSanitize
    , ppHidden   = pangoColor "white" . wrap "{" "}" . pangoSanitize
    , ppUrgent   = pangoColor "red"
    , ppLayout   = pangoColor "purple" . pangoSanitize
    , ppSep      = " "
    }

getWellKnownName :: D.Client -> IO ()
getWellKnownName dbus = do
  D.requestName dbus (D.busName_ "org.xmonad.Log")
                [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  return ()
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal "/org/xmonad/Log" "org.xmonad.Log" "Update") {
            D.signalBody = [D.toVariant ("<b>" ++ (UTF8.decodeString str) ++ "</b>")]
        }
    D.emit dbus signal

pangoColor :: String -> String -> String
pangoColor fg = wrap left right
  where
    left  = "<span foreground=\"" ++ fg ++ "\">"
    right = "</span>"

pangoSanitize :: String -> String
pangoSanitize = foldr sanitize ""
  where
    sanitize '>'  xs = "&gt;" ++ xs
    sanitize '<'  xs = "&lt;" ++ xs
    sanitize '\"' xs = "&quot;" ++ xs
    sanitize '&'  xs = "&amp;" ++ xs
    sanitize x    xs = x:xs

main :: IO ()
main = do
    dbus <- D.connectSession
    getWellKnownName dbus
    spawn "xfce4-panel --disable-wm-check"
    xmonad $ ewmh $ defaultConfig
        { manageHook = manageSpawn <> manageDocks <> manageHook defaultConfig <> myManageHook
        , layoutHook = smartBorders . avoidStruts $
--                       onWorkspace "1" Grid $
--                       onWorkspace "3" (Tall 1 (3/100) (6/7)) $
                       (multiCol [1] 0 0.01 (-0.5) ||| layoutHook defaultConfig)
        , logHook = dynamicLogWithPP (prettyPrinter dbus)
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        , terminal = "xfce4-terminal"
        , handleEventHook    = fullscreenEventHook
        , startupHook = do
            spawn "xfce4-power-manager"
            spawnOn "1" "xfce4-terminal -x weechat"
            spawn "light-locker"
            spawn "hipchat"
        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_z), spawn "light-locker-command -l")
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")
        , ((mod4Mask .|. shiftMask, xK_t), spawn "msu")
        , ((0, xF86XK_AudioLowerVolume), setMute False >> lowerVolume 4 >>= alert)
        , ((0, xF86XK_AudioRaiseVolume), setMute False >> raiseVolume 4 >>= alert)
        , ((0, xF86XK_AudioMute), toggleMute' >>= alert)
        , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -10")
        , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight +10")
        ]

myManageHook :: ManageHook
myManageHook = composeAll . concat $
    [ [ isFullscreen --> doFullFloat ]
    , [ className   =? c --> doFloat           | c <- myFloats]
    , [ title       =? t --> doFloat           | t <- myOtherFloats]
    , [ className   =? c --> doF (W.shift "2") | c <- webApps]
    , [ title       =? c --> doF (W.shift "1") | c <- chatApps]
    , [ className   =? c --> doF (W.shift "3") | c <- mailApps]
    ]
-- (Use the command xprop | grep WM_NAME to get the title property.)
  where myFloats      = ["MPlayer"]
        myOtherFloats = ["alsamixer"]
        webApps       = ["Firefox-bin", "Opera", "Chromium"] -- open on desktop 2
        chatApps      = ["WeeChat", "Pidgin", "HipChat"]     -- open on desktop 1
        mailApps      = ["Kmail", "Thunderbird"]             -- open on desktop 3
