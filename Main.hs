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
import qualified Data.ByteString.Lazy as BS
import           Data.Csv                 
                 
import           ReadData
import           Track

data TrackOpts = TrackOpts { file        :: FilePath
                           , maxDist     :: Dist
                           , maxTau      :: Int
                           , dt0         :: Int
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
    <*> option       ( long "max-tau"
                    <> short 'T'
                    <> value 3000
                    <> help "Maximum tau in frames"
                     )
    <*> option       ( long "dt0"
                    <> short 't'
                    <> value 10
                    <> help "Step of dt0"
                     )

tabSep = defaultEncodeOptions { encDelimiter = 9 }

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
    printf "Found %d tracks\n" (M.size tracks)
    forM_ (M.assocs tracks) $ \(TID k,v)->do
        BS.writeFile (printf "track-%03d.points" k)
            $ encodeWith tabSep $ V.map (\(t,P (R2 x y))->(t,x,y)) v
        BS.writeFile (printf "track-%03d.displ" k)
            $ encodeWith tabSep $ V.fromList
            $ map (\tau -> (tau, mean $ meanSqDispl (dt0 args) v tau)) 
            $ map round $ logspace 1 (realToFrac $ maxTau args) 100
    
logspace :: RealFloat a => a -> a -> Int -> [a]
logspace a b n = [ exp $ log a + realToFrac i * d | i <- [0..n-1]]
    where d = (log b - log a) / realToFrac n

meanSqDispl :: Time -> V.Vector DataPoint -> Time -> V.Vector Dist
meanSqDispl dt0 points tau =
    V.fromList $ mapMaybe f $ tailsV dt0 points
    where f :: V.Vector DataPoint -> Maybe Dist
          f v | V.null v'  = Nothing
              | otherwise  = let (_,x)  = V.head v
                                 (_,x') = V.head v'
                             in Just $ magnitudeSq $ x .-. x'
              where v' = V.drop tau v
    
tailsV :: Int -> V.Vector a -> [V.Vector a]            
tailsV n v | V.null v = []
           | otherwise = v : tailsV n (V.drop n v)

