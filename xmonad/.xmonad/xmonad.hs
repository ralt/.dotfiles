import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Actions.SpawnOn
import System.IO

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ defaultConfig
        { manageHook = manageDocks <+> manageHook defaultConfig <+> manageSpawn
        , layoutHook = avoidStruts  $  layoutHook defaultConfig
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        , terminal = "urxvtc"
        , logHook = dynamicLogWithPP xmobarPP
            { ppOutput = hPutStrLn xmproc
            , ppTitle = xmobarColor "green" "" . shorten 50
            }
        , startupHook = do
            spawnOn "1" "google-chrome"
            spawnOn "2" "xchat"
            spawnOn "3" "thunderbird"
            spawnOn "4" "empathy"
            spawnOn "4" "skype"
        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
        , ((mod4Mask .|. shiftMask, xK_x), spawn "sb")
        ]
