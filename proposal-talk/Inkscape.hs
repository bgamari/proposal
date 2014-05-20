{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Inkscape
    ( SvgFilter
    , LayerLabel
    , hideLayers
    , showOnlyLayers
    , highlightLayers
    , scale
    , runFilter
    ) where

import Prelude hiding (FilePath)

import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import qualified Data.Map as M
import Control.Monad.Trans.Class

import qualified Data.Text as T
import qualified Text.XML as Xml
import           Data.Text (Text)
import Text.XML.Lens
import Control.Error
import Data.Default
import Data.Default

import qualified Filesystem.Path as Path
import Filesystem.Path.CurrentOS (FilePath, decodeString, encodeString, toText)

type SvgFilter = Document -> Document
type LayerLabel = Text

inkscape :: Name -> Name
inkscape = _nameNamespace ?~ "http://www.inkscape.org/namespaces/inkscape"

svg :: Name -> Name
svg = _nameNamespace ?~ "http://www.w3.org/2000/svg"
 
allLayers :: Document -> [LayerLabel]
allLayers doc = catMaybes $ doc ^.. traverseGroups . attrs . at (inkscape "label")

traverseGroups :: Traversal' Document Element
traverseGroups =
    root
    . entire . filtered (views name (== svg "g"))
    . attributeIs (inkscape "groupmode") "layer"

showAllGroups :: Document -> Document
showAllGroups = traverseGroups . attrs . at "style" .~ Nothing

hideLayers :: [LayerLabel] -> SvgFilter
hideLayers layers doc =             
    let match el = (el ^. attrs . at (inkscape "label")) `elem` map Just layers
    in showAllGroups doc
       & traverseGroups
       . filtered match
       . style "display" ?~ "none"

showOnlyLayers :: [LayerLabel] -> SvgFilter
showOnlyLayers showLayers doc =             
    let match el = (el ^. attrs . at (inkscape "label")) `notElem` map Just showLayers
    in showAllGroups doc
       & traverseGroups
       . filtered match
       . style "display" ?~ "none"

highlightLayers :: Double -> [LayerLabel] -> SvgFilter
highlightLayers opacity showLayers doc =             
    let match el = (el ^. attrs . at (inkscape "label")) `notElem` map Just showLayers
    in doc & traverseGroups
           . filtered match
           . style "opacity" ?~ T.pack (show opacity)

type StyleAttr = Text

style :: StyleAttr -> Lens' Element (Maybe Text)
style s = attribute "style" . non T.empty . style' . at s

style' :: Iso' Text (M.Map StyleAttr Text)
style' = iso to from
  where
    splitKeyValue x = case T.splitOn ":" x of
                        [k,v]     -> M.singleton k v
                        otherwise -> M.empty
    to = M.unions . map splitKeyValue . T.splitOn ";" 
    from = T.intercalate ";" . map (\(k,v)->k<>":"<>v) . M.toList
        
scale :: Double -> Document -> Document
scale s doc = doc & root . nodes %~ scaleNodes
  where
    scaleNodes :: [Node] -> [Node]
    scaleNodes nodes =
      [NodeElement $ Element "g" scaleAttr nodes]
    scaleAttr = M.singleton "transform" (T.pack $ "scale("++show s++")")

runFilter :: FilePath -> FilePath -> SvgFilter -> EitherT String IO ()
runFilter inFile outFile transform = do
    doc <- lift $ Xml.readFile def inFile
    --let notFound = filter (\l->l `notElem` allLayers doc) showLayers
    --when (not $ null notFound) $ lift $ putStrLn $ "couldn't find layers: "++show notFound
    lift $ Xml.writeFile def outFile (transform doc)
