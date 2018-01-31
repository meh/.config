{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

import Data.Tuple

import XMonad hiding ( (|||) )
import XMonad.StackSet as W hiding (workspaces, focusDown, focusUp)
import XMonad.Config.Desktop
import XMonad.Util.EZConfig (additionalKeys, additionalKeysP, removeKeysP)
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.Submap

import XMonad.Layout.BoringWindows
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.LayoutScreens
import XMonad.Layout.IndependentScreens
import XMonad.Layout.Grid
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ResizableTile

conf = desktopConfig
    { terminal = "/home/meh/.cargo/bin/cancer"

    , handleEventHook = fullscreenEventHook
    , manageHook = composeAll
        [ className =? "Graphviz" --> doFloat
        , className =? "Gpicview" --> doFloat
        ]

    , workspaces = withScreens 3 (map show [1 .. 9])
    , layoutHook = windowNavigation $ boringWindows
        $ subLayout [0, 1] (Tip Top (3/100) 0.6 ||| Full)
        $ ThreeColMid 1 (3/100) (40/100) ||| Grid ||| Full ||| Tip Top (3/100) 0.2

    , normalBorderColor  = "#333333"
    , focusedBorderColor = "#b21818"
    }

main = xmonad $ conf
    -- Make workspaces per screen.
    `additionalKeys`
    [ ((m .|. modMask conf, k), windows $ onCurrentScreen f i)
    | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]

    -- Reorder screens.
    `additionalKeys`
    [ ((m .|. modMask conf, k), screenWorkspace s >>= flip whenJust (windows . f))
    | (k, s) <- zip [xK_w, xK_e, xK_r] [0, 2, 1]
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]

    -- Boring Windows.
    `additionalKeysP`
    [ ("M-j", focusDown)
    , ("M-k", focusUp)
    ]

    -- Sub Layouts.
    `additionalKeysP`
    [ ("M-C-h", sendMessage $ pullGroup L)
    , ("M-C-l", sendMessage $ pullGroup R)
    , ("M-C-k", sendMessage $ pullGroup U)
    , ("M-C-j", sendMessage $ pullGroup D)

    , ("M-C-m", withFocused (sendMessage . MergeAll))
    , ("M-C-u", withFocused (sendMessage . UnMerge))
    , ("M-C-S-u", withFocused (sendMessage . UnMergeAll))

    , ("M-C-<Period>", onGroup W.focusUp')
    , ("M-C-<Comma>", onGroup W.focusDown')

    , ("M-s", submap $ defaultSublMap conf)
    ]

    `additionalKeysP`
    [ ("M-C-<Return>", spawn "/home/meh/.cargo/bin/cancer --font 'GohuFont HiDPI 14px'")
    , ("M4-f", spawn "firefox-developer-edition")
    , ("M4-S-f", spawn "google-chrome-unstable")
    , ("M4-s", spawn "slack")
    , ("M4-S-s", spawn "spotify --force-device-scale-factor=1.2")

    , ("M-S-<Space>", layoutSplitScreen 2 (Tip Top (3/100) 0.2))
    , ("M-C-<Space>", rescreen)

    , ("M-<F1>", sendMessage $ JumpToLayout "ThreeColumns")
    , ("M-<F2>", sendMessage $ JumpToLayout "Grid")
    , ("M-<F3>", sendMessage $ JumpToLayout "Full")
    ]

    `removeKeysP`
    [ "M-S-q" ]

data Side = Top | Bottom
    deriving (Show, Read)

data Tip a =
    Tip Side Rational Rational
    deriving (Show, Read)

instance LayoutClass Tip a where
    doLayout (Tip side _ split) r s = return (arrange r s, Nothing)
        where
          arrange rect st = case reverse (W.up st) of
                              (master : _) -> [(master, first), (W.focus st, second)]
                              [] -> case W.down st of
                                      (next : _) -> [(W.focus st, first), (next, second)]
                                      [] -> [(W.focus st, rect)]
              where (first, second) = case side of
                                        Top -> splitVerticallyBy split rect
                                        Bottom -> swap $ splitVerticallyBy split rect

    handleMessage (Tip side delta split) x =
        return $ case fromMessage x of
                   Just Shrink -> Just (Tip side delta (split - delta))
                   Just Expand -> Just (Tip side delta (split + delta))
                   _           -> Nothing

    description (Tip Top _ _) = "TipTop"
    description (Tip Bottom _ _) = "TipBottom"
