{-# LANGUAGE OverloadedStrings, RankNTypes #-}

import Talk
import Inkscape
import Text.Pandoc.Builder hiding (note)
import qualified Data.Text as T
import qualified Filesystem.Path.CurrentOS
import Control.Monad.Writer

bullets = tell . bulletList . map (plain . text)

slides :: Talk ()
slides = do
   -- introduce single molecule measurement
   slide "Motivation" $ do
     svgFigure "A single-molecule" "figures/fret.svg"
       $ showOnlyLayers ["molecule", "molecule-label", "molecule-distance", "scale-bar"]
     note "we want to probe the shape or interactions of a biomolecular system"
     note "One powerful approach is to attach fluorescent probe"

   slide "FRET" $ do
     bullets [ "Fӧrster resonance energy transfer"
             , "A fluorescence spectroscopy"
             , "Provides insight into molecular geometry and kinetics" ]

   slide "FRET" $ do
     note "attach a pair of appropriately chosen luorophores to the molecule of interest"
     svgFigure "Seeing a single molecule" "figures/fret.svg"
       $ showOnlyLayers ["molecule", "scale-bar", "donor", "acceptor", "fluorophore-label"]

   slide "FRET" $ do
     svgFigure "Seeing a single molecule" "figures/fret.svg"
       $ showOnlyLayers ["molecule", "donor", "acceptor", "fluorophore-label", "donor-exc-photon"]
     note "illuminate the molecule with appropriate excitation"

   slide "FRET" $ do
     note "This excites the fluorophore"
     svgFigure "Seeing a single molecule" "figures/fret.svg"
       $ showOnlyLayers ["molecule", "excited-donor", "acceptor"]
   
   slide "FRET" $ do
     note "this can either decay, potentially with production of a photon"
     svgFigure "Seeing a single molecule" "figures/fret.svg"
       $ showOnlyLayers ["molecule", "donor", "donor-em-photon", "acceptor"]
   
   slide "FRET" $ do
     note "or"
     svgFigure "Seeing a single molecule" "figures/fret.svg"
       $ showOnlyLayers ["molecule", "excited-donor", "acceptor"]
   
   slide "FRET" $ do 
     note "will undergo Fӧrster energy transfer, donating its energy to the acceptor fluorophore"
     svgFigure "Measuring an intramolecular distance" "figures/fret.svg"
       $ showOnlyLayers ["molecule", "excited-donor", "donor-label", "acceptor", "acceptor-label", "energy-transfer"]

   slide "FRET" $ do 
     svgFigure "Measuring an intramolecular distance" "figures/fret.svg"
       $ showOnlyLayers ["molecule", "donor", "donor-label", "excited-acceptor", "acceptor-label", "energy-transfer"]
   

main = doTalk slides
