{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (FilePath)                
import System.IO

import Control.Error
import Control.Monad.Trans.Class
import Control.Monad.State
import Control.Monad.IO.Class
import Text.Pandoc
import Text.Pandoc.Walk
import Data.Default
import Control.Monad
import qualified Data.Text as T
import Filesystem.Path.CurrentOS

import Inkscape

main = runEitherT $ flip evalStateT 0 $ do
        liftIO getContents
    >>= return . readJSON def
    >>= walkM walkInline
    >>= return . writeJSON def
    >>= liftIO . putStr
 
walkInline :: Inline -> StateT Int (EitherT String IO) Inline 
walkInline (Image contents (fname,alt)) = do
    let (contents', filters) = partitionEithers $ map findFilterDef contents
    modify (+1)
    case filters of
      [] -> return $ Image contents (fname, alt)
      _  -> do
        n <- get
        let f = foldl (.) id filters
        let fname' = decodeString fname
            fnameNew = replaceExtension fname' (T.pack $ show n++"out.svg")
        lift $ runFilter fname' fnameNew f
        return $ Image contents' (encodeString fnameNew, alt)
  where
    findFilterDef :: Inline -> Either Inline SvgFilter
    findFilterDef (Code _ s) | "filter:":rest <- words s =
        case rest of
          "only":layers -> Right $ showOnlyLayers (map T.pack layers)
          x             -> error "unknown filter type"
    findFilterDef x = Left x

walkInline inline = return inline
