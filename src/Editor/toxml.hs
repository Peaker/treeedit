{-# OPTIONS -O2 -Wall #-}

module Main (main) where

import           Control.Monad                   (join)
import           Data.Store.Transaction          (Transaction)
import qualified Data.Store.Transaction          as Transaction
import qualified Data.Store.Property             as Property
import qualified Data.Store.Db                   as Db
import           System.IO                       (stdout, hPutStrLn, Handle)
import qualified Graphics.UI.VtyWidgets.TextEdit as TextEdit
import qualified Editor.Data                     as Data
import qualified Editor.Anchors                  as Anchors
import           Editor.Anchors                  (ViewTag, DBTag)
import qualified Data.Record.Label               as Label

type Action = IO ()

writeTreeXml :: Monad m =>
                Handle -> Int ->
                Transaction.Property ViewTag m Data.TreeD ->
                Transaction ViewTag m Action
writeTreeXml outFile depth ref = do
  value <- Property.get $ Data.nodeValue `Property.composeLabel` ref
  childrenIRefs <- Property.get (Data.nodeChildrenRefs `Property.composeLabel` ref)
  let text = TextEdit.textEditText . Label.getL Data.textEditModel $ value
      indent = (replicate (2 * depth) ' ' ++)
  let before = hPutStrLn outFile . indent $ "<" ++ text ++ ">"
  bodies <- mapM (writeTreeXml outFile (depth + 1) . Transaction.fromIRef) childrenIRefs
  let after = hPutStrLn outFile . indent $ "</" ++ text ++ ">"
  return . sequence_ $ [before] ++ bodies ++ [after]

printXml :: Transaction DBTag IO Action
printXml = do
  view <- Property.get Anchors.view
  Transaction.run (Anchors.viewStore view) $
    writeTreeXml stdout 0 Anchors.root

main :: IO ()
main = Db.withDb "/tmp/treeedit.db" showXml
  where
    showXml db = do
      Anchors.initDB store
      join $ Transaction.run store printXml
      where
        store = Anchors.dbStore db