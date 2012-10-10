{-# LANGUAGE OverloadedStrings #-}

import           Data.Foldable
import           Data.Maybe
import           Data.Monoid
import           Options.Applicative

import qualified Data.Vector as V
import           Data.Vector (Vector)
import qualified Data.Map as M

import           Data.VectorSpace
import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Statistics.Sample                 

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
        TL.writeFile (printf "track-%03d" k) $ TB.toLazyText $ formatTrack v
        print (k, meanVariance $ meanSqDispl v 10)
    
formatTrack :: V.Vector DataPoint -> TB.Builder
formatTrack = foldMap formatPoint 
    where formatPoint (t,P (x,y)) = fmt x <> "\t" <> fmt y <> "\n"
          fmt = formatRealFloat Generic (Just 3)

meanSqDispl :: V.Vector DataPoint -> Time -> V.Vector Dist
meanSqDispl points tau =
    V.fromList $ mapMaybe f $ tailsV points
    where f :: V.Vector DataPoint -> Maybe Dist
          f v | V.null v'  = Nothing
              | otherwise  = let (_,x)  = V.head v
                                 (_,x') = V.head v'
                             in Just $ magnitudeSq $ x .-. x'
              where v' = V.drop tau v
    
tailsV :: V.Vector a -> [V.Vector a]            
tailsV v | V.null v = []
         | otherwise = v : tailsV (V.tail v)
   
 
