{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Data.Monoid
import XMonad
import XMonad.Config.Kde
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.MultiColumns
import XMonad.Layout.NoBorders
import qualified XMonad.StackSet as W -- to shift and float windows

main :: IO ()
main = do
    xmonad $ kde4Config
        { manageHook = manageHook kde4Config <> myManageHook
        , layoutHook = smartBorders . avoidStruts $
            (multiCol [1] 0 0.01 (-0.5) ||| layoutHook def)
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        , terminal = "gnome-terminal"
        , handleEventHook =
            handleEventHook kde4Config <> fullscreenEventHook <> ewmhDesktopsEventHook
        , startupHook = do
            startupHook kde4Config
            ewmhDesktopsStartup
            setWMName "LG3D"
        }

myManageHook :: ManageHook
myManageHook = composeAll . concat $
    [ [ isFullscreen --> doFullFloat ]
    , [ className   =? c --> doFloat           | c <- myFloats]
    , [ title       =? t --> doFloat           | t <- myOtherFloats]
    , [ className   =? c --> doF (W.shift "2") | c <- webApps]
    , [ className   =? c --> doF (W.shift "1") | c <- chatApps]
    , [ className   =? c --> doF (W.shift "3") | c <- mailApps]
    ]
-- (Use the command xprop | grep WM_NAME to get the title property.)
  where myFloats      = ["MPlayer"]
        myOtherFloats = ["alsamixer"]
        webApps       = ["Firefox-bin", "Opera", "Chromium"] -- open on desktop 2
        chatApps      = ["WeeChat", "Pidgin", "HipChat"]     -- open on desktop 1
        mailApps      = ["Kmail", "Thunderbird"]             -- open on desktop 3
