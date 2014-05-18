{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Inkscape
    ( LayerLabel
    , hideLayers
    , showOnlyLayers
    , highlightLayers
    ) where

import Prelude hiding (FilePath)

import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import qualified Data.Map as M

import qualified Data.Text as T
import qualified Text.XML as Xml
import           Data.Text (Text)
import Text.XML.Lens
import Control.Error
import Data.Default
import Data.Default

import qualified Filesystem.Path as Path
import Filesystem.Path.CurrentOS (FilePath, decodeString, encodeString, toText)

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

hideLayers :: [LayerLabel] -> Document -> Document
hideLayers layers doc =             
    let match el = (el ^. attrs . at (inkscape "label")) `elem` map Just layers
    in showAllGroups doc
       & traverseGroups
       . filtered match
       . style "display" ?~ "none"

showOnlyLayers :: [LayerLabel] -> Document -> Document
showOnlyLayers showLayers doc =             
    let match el = (el ^. attrs . at (inkscape "label")) `notElem` map Just showLayers
    in showAllGroups doc
       & traverseGroups
       . filtered match
       . style "display" ?~ "none"

highlightLayers :: Double -> [LayerLabel] -> Document -> Document
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
        
