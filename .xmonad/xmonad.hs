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
main =
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
    , [ className   =? c --> doF (W.shift "1") | c <- chatApps]
    , [ className   =? c --> doF (W.shift "2") | c <- webApps]
    , [ className   =? c --> doF (W.shift "3") | c <- mailApps]
    , [ className   =? c --> doF (W.shift "5") | c <- mediaApps]
    , [ className   =? c --> doF (W.shift "6") | c <- gameApps]
    ]
-- (Use the command xprop | grep WM_NAME to get the title property.)
  where
      myFloats = ["plasmashell"{-Volume change-}]
      chatApps = []
      webApps = ["google-chrome"]
      mailApps = []
      gameApps = ["Steam"]
      mediaApps = ["spotify","Spotify"]
