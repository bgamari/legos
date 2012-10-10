{-# LANGUAGE OverloadedStrings #-}

import           Data.Foldable    
import           Options.Applicative
import           Data.Monoid

import qualified Data.Vector as V
import           Data.Vector (Vector)
import qualified Data.Map as M

import           Data.VectorSpace
import           Data.AffineSpace.Point

import           Text.Printf
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Builder as TB
import           Data.Text.Lazy.Builder.RealFloat
                 
import           ReadData
import           Track

data TrackOpts = TrackOpts { file        :: FilePath
                           , maxDist     :: Dist
                           }
               deriving (Show, Eq)
               
trackOpts = TrackOpts
    <$> argument str ( metavar "FILE"
                    <> help "Particles file"
                     )
    <*> option       ( long "max-dist"
                    <> value 20
                    <> short 'd'
                    <> help "Maximum distance"
                     )
                 
main = do
    args <- execParser $ info (helper <*> trackOpts)
           ( fullDesc
          <> progDesc "Track those legos!"
          <> header "hep"
           )

    Right framePts <- readFramePts $ file args
    printf "Read %d points\n" (V.length framePts)
    let dps = V.map (\fp->(fpId fp, fpPos fp)) framePts
    let tracks = track 20 dps
    print $ M.keys tracks
    forM_ (M.assocs tracks) $ \(TID k,v)->do
        TL.writeFile (printf "track-%03d" k) $ TB.toLazyText $ "%f\t%f\n"
    
formatTrack :: [DataPoint] -> TB.Builder
formatTrack = foldMap formatPoint 
    where formatPoint (t,P (x,y)) = fmt x <> "\t" <> fmt y <> "\n"
          fmt = formatRealFloat Generic (Just 3)

