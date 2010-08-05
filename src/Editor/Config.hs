{-# OPTIONS -fno-warn-missing-signatures #-}

module Editor.Config(
    maxDepth,
    quitKeys, undoKeys, goUpKeys, appendChildKeys, delChildKeys,
    setFocalPointKeys, cutKeys, pasteKeys,
    makeBranchKeys, delBranchKeys,
    actionKeys, collapseKeys, expandKeys, moveToParentKeys)
where

import qualified Graphics.Vty as Vty
import qualified Graphics.UI.VtyWidgets.Keymap as Keymap

maxDepth :: Int
maxDepth = 10

group mods k = Keymap.simpletonGroup (mods, k)
ascii k = group [] (Vty.KASCII k)
ctrl k = group [Vty.MCtrl] (Vty.KASCII k)

quitKeys          = [ctrl 'q']
undoKeys          = [ctrl 'z']
goUpKeys          = [ctrl 'r']
appendChildKeys   = [ctrl 'n']
delChildKeys      = [ctrl 'o']
setFocalPointKeys = [ctrl 'g']
pasteKeys         = [ctrl 'v']
cutKeys           = [ctrl 'x']
makeBranchKeys    = [ctrl 's']
delBranchKeys     = [ctrl 'o']
actionKeys        = [group [] Vty.KEnter]
collapseKeys      = [ascii '[']
expandKeys        = [ascii ']']
moveToParentKeys  = [group [Vty.MMeta] Vty.KLeft]
