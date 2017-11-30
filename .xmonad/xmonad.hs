{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

import XMonad hiding ( (|||) )
import XMonad.StackSet as W hiding (workspaces)
import XMonad.Config.Desktop
import XMonad.Util.EZConfig (additionalKeys, additionalKeysP, removeKeysP)

import XMonad.Layout.LayoutCombinators
import XMonad.Layout.LayoutScreens
import XMonad.Layout.IndependentScreens
import XMonad.Layout.Grid
import XMonad.Layout.ThreeColumns

conf = desktopConfig
    { terminal = "/home/meh/.cargo/bin/cancer"

    , manageHook = composeAll
        [ className =? "Graphviz" --> doFloat
        , className =? "Gpicview" --> doFloat
        ]

    , workspaces = withScreens 3 (map show [1 .. 9])
    , layoutHook = ThreeColMid 1 (3/100) (40/100) ||| Grid ||| Full

    , normalBorderColor  = "#333333"
    , focusedBorderColor = "#b21818"
    }

main = xmonad $ conf
    `additionalKeys`
    [ ((m .|. modMask conf, k), windows $ onCurrentScreen f i)
    | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]

    `additionalKeysP`
    [ ("M-C-<Return>", spawn "/home/meh/.cargo/bin/cancer --font 'GohuFont HiDPI 14px'")
    , ("M4-f", spawn "firefox-developer")
    , ("M4-s", spawn "slack")
    , ("M4-S-s", spawn "spotify --force-device-scale-factor=1.2")

    , ("M-S-<Space>", layoutSplitScreen 2 (Tip (3/100) 0.2))
    , ("M-C-<Space>", rescreen)

    , ("M-<F1>", sendMessage $ JumpToLayout "ThreeColumns")
    , ("M-<F2>", sendMessage $ JumpToLayout "Grid")
    , ("M-<F3>", sendMessage $ JumpToLayout "Full")
    ]

    `removeKeysP`
    [ "M-S-q"
    ]

data Tip a =
    Tip Rational Rational
    deriving (Show, Read)

instance LayoutClass Tip a where
    doLayout (Tip _ split) r s = return (arrange r s, Nothing)
        where
          arrange rect st = case reverse (W.up st) of
                              (master : _) -> [(master, top), (W.focus st, bottom)]
                              [] -> case W.down st of
                                      (next : _) -> [(W.focus st, top), (next, bottom)]
                                      [] -> [(W.focus st, rect)]
              where (top, bottom) = splitVerticallyBy split rect

    handleMessage (Tip delta split) x =
        return $ case fromMessage x of
                   Just Shrink -> Just (Tip delta (split - delta))
                   Just Expand -> Just (Tip delta (split + delta))
                   _           -> Nothing

    description _ = "Tip"
