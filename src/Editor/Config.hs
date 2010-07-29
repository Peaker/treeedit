{-# OPTIONS -fno-warn-missing-signatures #-}

module Editor.Config(
    maxDepth,
    quitKey, undoKey, goUpKey, appendChildKey, delChildKey,
    setFocalPointKey, cutKey, pasteKey,
    makeBranchKey, delBranchKey,
    actionKey, collapseKey, expandKey, moveToParentKey)
where

import qualified Graphics.Vty as Vty

maxDepth :: Int
maxDepth = 10

quitKey = ([Vty.MCtrl], Vty.KASCII 'q')
undoKey = ([Vty.MCtrl], Vty.KASCII 'z')
goUpKey = ([Vty.MCtrl], Vty.KASCII 'r')
appendChildKey = ([Vty.MCtrl], Vty.KASCII 'n')
delChildKey = ([Vty.MCtrl], Vty.KASCII 'o')
setFocalPointKey = ([Vty.MCtrl], Vty.KASCII 'g')
pasteKey = ([Vty.MCtrl], Vty.KASCII 'v')
cutKey = ([Vty.MCtrl], Vty.KASCII 'x')
makeBranchKey = ([Vty.MCtrl], Vty.KASCII 's')
delBranchKey = ([Vty.MCtrl], Vty.KASCII 'o')
actionKey = ([], Vty.KEnter)
collapseKey = ([], Vty.KASCII '[')
expandKey = ([], Vty.KASCII ']')
moveToParentKey = ([Vty.MMeta], Vty.KLeft)
