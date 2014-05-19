{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (FilePath)

import Control.Error
import Control.Monad.Trans.Class
import Control.Monad.State
import Control.Monad.IO.Class
import Data.Monoid ((<>))
import Text.Pandoc
import Text.Pandoc.Walk
import Data.Default
import Control.Monad
import qualified Data.Text as T
import Filesystem.Path.CurrentOS
import System.Process

import Inkscape

main = runEitherT $ flip evalStateT 0 $ do
        liftIO getContents
    >>= return . readJSON def
    >>= walkM walkFilters
    >>= walkM (lift . svgToPdf)
    >>= return . writeJSON def
    >>= liftIO . putStr

mapFileName :: (T.Text -> T.Text) -> FilePath -> FilePath
mapFileName f fpath = (directory fpath <> fname') `addExtensions` extensions fpath
  where fname' = fromText $ f $ either (error "invalid file name") id $ toText
               $ dropExtensions $ filename fpath

walkFilters :: Inline -> StateT Int (EitherT String IO) Inline 
walkFilters (Image contents (fname,alt)) = do
    let (contents', filters) = partitionEithers $ map findFilterDef contents
    modify (+1)
    case filters of
      [] -> return $ Image contents (fname, alt)
      _  -> do
        n <- get
        let f = foldl (.) id filters
        let fname' = decodeString fname
            fnameNew = mapFileName (<>T.pack ("-"++show n)) fname'
        lift $ runFilter fname' fnameNew f
        return $ Image contents' (encodeString fnameNew, alt)
  where
    findFilterDef :: Inline -> Either Inline SvgFilter
    findFilterDef (Code _ s) | "filter:":rest <- words s =
        case rest of
          "only":layers -> Right $ showOnlyLayers (map T.pack layers)
          x             -> error "unknown filter type"
    findFilterDef x = Left x
walkFilters inline = return inline

svgToPdf :: Inline -> EitherT String IO Inline 
svgToPdf (Image contents (fname,alt)) | fname' `hasExtension` "svg" = do
    let fnameNew = replaceExtension fname' "pdf"
    liftIO $ callProcess "inkscape" [fname, "--export-pdf", encodeString fnameNew]
    return $ Image contents (encodeString fnameNew, alt)
  where fname' = decodeString fname
svgToPdf inline = return inline
            
