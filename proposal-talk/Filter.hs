{-# LANGUAGE OverloadedStrings, TemplateHaskell, RankNTypes #-}
import Prelude hiding (FilePath)

import Control.Error
import Control.Monad.Trans.Class
import Control.Monad.State
import Control.Monad.IO.Class
import Data.Monoid
import Text.Pandoc
import Text.Pandoc.Walk
import Data.Default
import Control.Monad
import qualified Data.Text as T
import Filesystem.Path.CurrentOS
import System.Process
import Control.Lens

import qualified Data.Map as M
import qualified Data.Set as S

import Inkscape

data FilterState = FilterState { _figNum :: Int
                               , _visLayers :: M.Map FilePath (S.Set LayerLabel)
                               }
makeLenses ''FilterState

instance Default FilterState where
    def = FilterState {_figNum = 0, _visLayers = mempty}

main = runEitherT $ flip evalStateT def $ do
        liftIO getContents
    >>= return . readJSON def
    >>= walkM walkFilters
    >>= walkM (lift . svgToPdf)
    >>= return . walk filterNotes
    >>= return . writeJSON def
    >>= liftIO . putStr

mapFileName :: (T.Text -> T.Text) -> FilePath -> FilePath
mapFileName f fpath = (directory fpath <> fname') `addExtensions` extensions fpath
  where fname' = fromText $ f $ either (error "invalid file name") id $ toText
               $ dropExtensions $ filename fpath

visLayersFor :: FilePath -> Lens' FilterState (S.Set LayerLabel)
visLayersFor fname = visLayers . at fname . non S.empty

walkFilters :: Inline -> StateT FilterState (EitherT String IO) Inline 
walkFilters (Image contents (fname,alt)) = do
    (contents', filters) <- partitionEithers `fmap` mapM findFilterDef contents
    figNum += 1
    case filters of
      [] -> return $ Image contents (fname, alt)
      _  -> do
        n <- use figNum
        let f = foldl (.) id filters
        let fnameNew = mapFileName (<>T.pack ("-"++show n)) fname'
        lift $ runFilter fname' fnameNew f
        return $ Image contents' (encodeString fnameNew, alt)
  where
    fname' = decodeString fname
    findFilterDef :: Monad m => Inline -> StateT FilterState (EitherT String m) (Either Inline SvgFilter)
    findFilterDef (Code _ s) | "filter:":rest <- words s =
        case rest of
          "only":layers -> do let layers' = map T.pack layers
                              visLayersFor fname' .= S.fromList layers'
                              return $ Right $ showOnlyLayers layers'
          "last":layers -> do
            let layers' = map T.pack layers
                adds = S.fromList $ mapMaybe ("+" `T.stripPrefix`) layers'  -- add to visible layers
                dels = S.fromList $ mapMaybe ("-" `T.stripPrefix`) layers'  -- remove from visible layers
                hide = S.fromList $ mapMaybe ("!" `T.stripPrefix`) layers'  -- hide for just this frame
                show = S.fromList $ filter (\x->T.head x `notElem` "+-!") layers'
            visLayersFor fname' %= \xs->xs `S.union` adds `S.difference` dels
            vis <- use $ visLayersFor fname'
            return $ Right $ showOnlyLayers $ S.toList $ vis `S.difference` hide `S.union` show
          "scale":s:_ -> do
            s' <- lift $ tryRead "Invalid scale factor" s
            return $ Right $ scale s'
          x:_ -> lift $ left $ "unknown filter type: "<>x
    findFilterDef x = return $ Left x
walkFilters inline = return inline

svgToPdf :: Inline -> EitherT String IO Inline 
svgToPdf (Image contents (fname,alt)) | fname' `hasExtension` "svg" = do
    let fnameNew = replaceExtension fname' "pdf"
    liftIO $ callProcess "inkscape" [fname, "--export-pdf", encodeString fnameNew]
    return $ Image contents (encodeString fnameNew, alt)
  where fname' = decodeString fname
svgToPdf inline = return inline
            
filterNotes :: Block -> Block
filterNotes (OrderedList (0,_,_) _) = Null
filterNotes blk = blk            
    
