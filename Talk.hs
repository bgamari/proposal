{-# LANGUAGE OverloadedStrings, RankNTypes, TemplateHaskell #-}

module Talk where

import Prelude hiding (FilePath)
import Filesystem.Path.CurrentOS (FilePath, decodeString, encodeString, toText)
import qualified Filesystem.Path as Path
import Data.Monoid

import Control.Lens
import Text.XML.Lens hiding (text)
import qualified Text.XML as Xml
import Data.Default

import Control.Error hiding (note)
import Control.Monad.Trans
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State
import Control.Monad (when, forM)

import           Data.Text (Text)
import qualified Data.Text.IO as T

import Text.Pandoc.Builder
import Text.Pandoc

data TalkState = TalkState { _figureN :: Int }
makeLenses ''TalkState

type Talk = WriterT Blocks (StateT TalkState (EitherT String IO))

slide :: String -> Talk () -> Talk ()
slide title body = do
    tell $ header 1 (text title)
    body' <- lift $ execWriterT body
    tell body'

note :: String -> Talk ()
note t = return ()

figure :: String -> FilePath -> Talk ()
figure caption fname = do
    figureN += 1
    tell $ plain $ image (encodeString fname) caption mempty

svgFigure :: String -> FilePath -> (Document -> Document) -> Talk () 
svgFigure caption fname transform = do
    n <- use figureN
    let figName = decodeString $ "slide-"<>show n
    lift $ lift $ process fname (Path.addExtension figName "svg") transform
    let figOutName = Path.addExtension figName "pdf"
    figure caption figOutName

process :: FilePath -> FilePath -> (Document -> Document) -> EitherT String IO ()
process inFile outFile transform = do
    doc <- liftIO $ Xml.readFile def inFile
    --let notFound = filter (\l->l `notElem` allLayers doc) showLayers
    --when (not $ null notFound) $ lift $ putStrLn $ "couldn't find layers: "++show notFound
    liftIO $ Xml.writeFile def outFile (transform doc)

doTalk :: Talk () -> IO (Either String ())
doTalk talk = runEitherT $ do
    blks <- evalStateT (execWriterT talk) (TalkState 0)
    --liftIO $ putStrLn $ writeHtmlString def (doc blks)
    liftIO $ writeFile "talk.mkd" $ writeMarkdown def (doc blks)
