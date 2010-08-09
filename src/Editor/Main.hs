{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TypeOperators, Rank2Types #-}

module Main(main) where

import           Prelude                          hiding ((.))
import           Control.Category                 ((.))
import           Control.Monad                    (when, liftM)
import           Data.Store.IRef                  (IRef)
import qualified Data.Store.Transaction           as Transaction
import           Data.Store.Transaction           (Transaction)
import           Data.Store.Property              (composeLabel)
import qualified Data.Store.Property              as Property
import qualified Data.Store.Rev.Version           as Version
import qualified Data.Store.Rev.Branch            as Branch
import           Data.Store.Rev.View              (View)
import qualified Data.Store.Rev.View              as View
import           Data.Store.VtyWidgets            (MWidget, widgetDownTransaction, makeTextEdit, makeBox, appendBoxChild, popCurChild, makeChoiceWidget)
import           Data.Monoid                      (Monoid(..))
import           Data.Maybe                       (fromMaybe, fromJust)
import qualified Graphics.Vty                     as Vty
import qualified Graphics.UI.VtyWidgets.TextView  as TextView
import qualified Graphics.UI.VtyWidgets.TextEdit  as TextEdit
import qualified Graphics.UI.VtyWidgets.Box       as Box
import qualified Graphics.UI.VtyWidgets.Spacer    as Spacer
import qualified Graphics.UI.VtyWidgets.Widget    as Widget
import qualified Graphics.UI.VtyWidgets.Keymap    as Keymap
import           Graphics.UI.VtyWidgets.Widget    (Widget)
import qualified Graphics.UI.VtyWidgets.Run       as Run
import qualified Data.Store.Db                    as Db
import           Editor.Data                      (ITreeD, TreeD)
import qualified Editor.Data                      as Data
import qualified Editor.Anchors                   as Anchors
import           Editor.Anchors                   (DBTag, ViewTag)
import qualified Editor.Config                    as Config

focusableTextView :: String -> Widget a
focusableTextView =
  Widget.coloredFocusableDisplay Vty.blue .
  TextView.make Vty.def_attr

removeAt :: Int -> [a] -> [a]
removeAt n xs = take n xs ++ drop (n+1) xs

makeChildBox :: Monad m =>
                Int ->
                Transaction.Property ViewTag m [ITreeD] ->
                Transaction.Property ViewTag m Box.Model ->
                Transaction.Property ViewTag m Box.Model ->
                Transaction.Property ViewTag m [ITreeD] ->
                Transaction ViewTag m (Widget (Transaction ViewTag m ()))
makeChildBox depth clipboardRef outerBoxModelRef childrenBoxModelRef childrenIRefsRef = do
  childItems <- mapM (makeTreeEdit (depth+1) clipboardRef) =<< Property.get childrenIRefsRef
  curChildIndex <- getChildIndex . length $ childItems
  childBox <- makeBox Box.Vertical childItems childrenBoxModelRef
  return .
    Widget.weakerKeys
    (mappend
     delNodeKeymap cutNodeKeymap
     curChildIndex) .
    Widget.atDisplay (Spacer.indent 5) $
    childBox
  where
    cutNodeKeymap = fromMaybe mempty .
                    liftM (Keymap.fromKeyGroups Config.cutKeys "Cut node" . cutChild)
    delNodeKeymap = fromMaybe mempty .
                    liftM (Keymap.fromKeyGroups Config.delChildKeys "Del node" . delChild)
    cutChild index = do
      childrenIRefs <- Property.get childrenIRefsRef
      Property.pureModify clipboardRef (childrenIRefs !! index :)
      delChild index
    delChild index = do
      Property.pureModify childrenIRefsRef $ removeAt index
      isEmpty <- null `liftM` Property.get childrenIRefsRef
      when isEmpty . Property.set outerBoxModelRef . Box.Model $ 0
    getChildIndex count = (validateIndex count . Box.modelCursor) `liftM`
                          Property.get childrenBoxModelRef
    validateIndex count index
      | 0 <= index && index < count = Just index
      | otherwise = Nothing

simpleTextEdit :: Monad m =>
                  Transaction.Property t m TextEdit.Model ->
                  MWidget (Transaction t m)
simpleTextEdit = makeTextEdit "<empty>" 1 TextEdit.defaultAttr TextEdit.editingAttr

makeTreeEdit :: Monad m =>
                Int -> Transaction.Property ViewTag m [ITreeD] ->
                IRef TreeD ->
                Transaction ViewTag m (Widget (Transaction ViewTag m ()))
makeTreeEdit depth clipboardRef treeIRef
  | depth >= Config.maxDepth =
    return $ Widget.strongerKeys goInKeymap $ focusableTextView "[Go deeper]"
  | otherwise = do
    valueEdit <- (Widget.atKeymap . Keymap.removeKeyGroups . concat)
                 [Config.expandKeys, Config.collapseKeys]
                 `liftM`
                 simpleTextEdit valueTextEditModelRef
    isExpanded <- Property.get isExpandedRef
    lowRow <- if isExpanded
              then ((:[]) .
                    Widget.weakerKeys moveToParentKeymap) `liftM`
                   makeChildBox depth clipboardRef outerBoxModelRef childrenBoxModelRef childrenIRefsRef
              else return []
    cValueEdit <- makeBox Box.Horizontal
                  [collapser isExpanded,
                   Widget.simpleDisplay $ Spacer.makeWidthSpace 1,
                   valueEdit]
                  (treeNodeBoxModelRef 2) -- 2 points to valueEdit
    outerBox <- makeBox Box.Vertical (cValueEdit : lowRow) outerBoxModelRef
    clipboard <- Property.get clipboardRef
    let keymap =
          mconcat [
            pasteKeymap clipboard,
            appendNewNodeKeymap,
            setFocalPointKeymap,
            expandCollapseKeymap isExpanded
            ]
    return . Widget.weakerKeys keymap $ outerBox
    where
      goInKeymap = Keymap.fromKeyGroups Config.actionKeys "Go deeper" setFocalPoint
      treeRef = Transaction.fromIRef treeIRef
      valueRef = Data.nodeValue `composeLabel` treeRef
      boxModelsContainer def k = Data.boxModel def k `composeLabel` valueRef
      valueTextEditModelRef = Data.textEditModel `composeLabel` valueRef
      childrenIRefsRef = Data.nodeChildrenRefs `composeLabel` treeRef
      isExpandedRef = Data.isExpanded `composeLabel` valueRef
      outerBoxModelRef = boxModelsContainer Box.initModel "outer"
      treeNodeBoxModelRef initValue = boxModelsContainer (Box.Model initValue) "treeNode"
      childrenBoxModelRef = boxModelsContainer Box.initModel "children"
      expandCollapseKeymap isExpanded =
        if isExpanded
        then Keymap.fromKeyGroups Config.collapseKeys "Collapse" collapse
        else Keymap.fromKeyGroups Config.expandKeys "Expand" expand
      collapse = Property.set isExpandedRef False
      expand = Property.set isExpandedRef True
      collapser isExpanded =
        Widget.simpleDisplay .
        TextView.make Vty.def_attr $
        if isExpanded
        then "[-]"
        else "[+]"
      pasteKeymap [] = mempty
      pasteKeymap (cbChildRef:xs) =
        Keymap.fromKeyGroups Config.pasteKeys "Paste" $ do
          appendChild cbChildRef
          Property.set clipboardRef xs
      appendNewNodeKeymap = Keymap.fromKeyGroups Config.appendChildKeys "Append new child node" $
                            appendChild =<< Data.makeLeafRef ""
      moveToParentKeymap = Keymap.fromKeyGroups Config.moveToParentKeys "Move to parent" .
                           Property.set outerBoxModelRef $ Box.Model 0
      setFocalPointKeymap = Keymap.fromKeyGroups Config.setFocalPointKeys "Set focal point" $ setFocalPoint
      setFocalPoint = Property.pureModify Anchors.focalPointIRefs (treeIRef:)
      appendChild newRef = do
        appendBoxChild childrenBoxModelRef childrenIRefsRef newRef
        Property.set outerBoxModelRef $ Box.Model 1

makeEditWidget :: Monad m =>
                  Transaction.Property ViewTag m [ITreeD] ->
                  Transaction ViewTag m (Widget (Transaction ViewTag m ()))
makeEditWidget clipboardRef = do
  focalPointIRefs <- Property.get focalPointIRefsRef
  treeEdit <- makeTreeEdit 0 clipboardRef (foldr const Anchors.rootIRef focalPointIRefs)
  widget <-
    if not $ isAtRoot focalPointIRefs
    then makeBox Box.Vertical [goUpButton, treeEdit] (Anchors.viewBoxsAnchor "goUp")
    else return treeEdit
  return .
    Widget.strongerKeys (goUpKeymap focalPointIRefs) $
    widget
  where
    goUpButton = Widget.strongerKeys (Keymap.fromKeyGroups Config.actionKeys "Go up" goUp) $
                 focusableTextView "[go up]"
    focalPointIRefsRef = Anchors.focalPointIRefs
    isAtRoot = null
    goUpKeymap focalPointIRefs =
      if isAtRoot focalPointIRefs
      then mempty
      else Keymap.fromKeyGroups Config.goUpKeys "Go up" goUp
    goUp = Property.pureModify focalPointIRefsRef (drop 1)

branchSelectorBoxModel :: Monad m => Transaction.Property DBTag m Box.Model
branchSelectorBoxModel = Anchors.dbBoxsAnchor "branchSelector"

-- Apply the transactions to the given View and convert them to
-- transactions on a DB
makeWidgetForView :: Monad m => View -> Transaction DBTag m (Widget (Transaction DBTag m ()))
makeWidgetForView view = do
  versionData <- Version.versionData =<< View.curVersion view
  widget <- widgetDownTransaction (Anchors.viewStore view) $
            makeEditWidget Anchors.clipboard
  return $ Widget.strongerKeys (keymaps versionData) widget
  where
    keymaps versionData = undoKeymap versionData `mappend` makeBranchKeymap
    makeBranchKeymap = Keymap.fromKeyGroups Config.makeBranchKeys "New Branch" makeBranch
    makeBranch = do
      branch <- Branch.new =<< View.curVersion view
      textEditModelIRef <- Transaction.newIRef $ TextEdit.initModel "New view"
      let viewPair = (textEditModelIRef, branch)
      appendBoxChild branchSelectorBoxModel Anchors.branches viewPair
    undoKeymap versionData =
        if Version.depth versionData > 1
        then Keymap.fromKeyGroups Config.undoKeys "Undo" .
             View.move view .
             fromJust . Version.parent $
             versionData
        else mempty

main :: IO ()
main = Db.withDb "/tmp/treeedit.db" $ runDbStore . Anchors.dbStore
  where
    runDbStore store = do
      Anchors.initDB store
      Run.widgetLoopWithOverlay 20 30 . const . makeWidget $ store
    makeWidget dbStore = widgetDownTransaction dbStore $ do
      view <- Property.get Anchors.view
      branches <- Property.get Anchors.branches
      pairs <- mapM pair branches
      (branchSelector, branch) <- makeChoiceWidget Box.Vertical pairs branchSelectorBoxModel
      View.setBranch view branch
      viewEdit <- Widget.strongerKeys quitKeymap
                  `liftM` makeWidgetForView view
      makeBox Box.Horizontal
        [viewEdit,
         Widget.simpleDisplay Spacer.makeHorizontal,
         Widget.strongerKeys (delBranchKeymap branches) branchSelector] $
        Anchors.dbBoxsAnchor "main"

    pair (textEditModelIRef, version) = do
      textEdit <- simpleTextEdit . Transaction.fromIRef $ textEditModelIRef
      return (textEdit, version)

    delBranchKeymap [_] = mempty
    delBranchKeymap _ = Keymap.fromKeyGroups Config.delBranchKeys "Delete Branch" deleteCurBranch
    quitKeymap = Keymap.fromKeyGroups Config.quitKeys "Quit" . fail $ "Quit"

    deleteCurBranch = do
      _ <- popCurChild branchSelectorBoxModel Anchors.branches
      return ()
