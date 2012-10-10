{-# LANGUAGE TupleSections, TemplateHaskell, GeneralizedNewtypeDeriving #-}

module Track ( track
             , R2, Time, Dist, DataPoint
             , TrackID(..)
             ) where

import           Data.Foldable (forM_)       

import qualified Data.Vector as V                
import qualified Data.Map as M
import           Data.Map (Map)
import           Data.Function (on)
import           Data.List
import           Data.Maybe
import           Data.VectorSpace
import           Data.AffineSpace
import           Data.AffineSpace.Point

import           Control.Monad.State.Strict hiding (forM_)
import           Data.Lens.Common
import           Data.Lens.Strict
import           Data.Lens.Template

import Debug.Trace
                 
type R2 = (Double, Double)       
type Time = Int
type Dist = Double
newtype TrackID = TID Int
                deriving (Show, Ord, Eq, Num)
type DataPoint = (Time, Point R2)


backtracks :: Map TrackID [DataPoint] -> [Map TrackID DataPoint]
backtracks trks
    | M.null trks  = []
    | otherwise    = fmap head trks : ( backtracks 
                                        $ M.filter (not . null)
                                        $ fmap tail trks
                                      )
    
findTrack :: Int -> Dist -> Map TrackID [DataPoint] -> DataPoint -> Maybe TrackID
findTrack maxDepth maxDist trks (t,pt) = f $ take maxDepth $ backtracks trks
    where f :: [Map TrackID (Time, Point R2)] -> Maybe TrackID
          f [] = Nothing
          f (x:rest) =
              let proposals :: [(TrackID, Dist)]
                  proposals = M.assocs 
                            $ M.filter (< maxDist)
                            $ fmap (\(t,p)->pt `distance` p) x
              in case proposals of
                     []  -> f rest
                     xs  -> let (tid,dist) = minimumBy (compare `on` snd) xs
                                  in Just tid
    
data TrackState = TState { _lastTrack :: !TrackID
                         , _tracks    :: !(Map TrackID [DataPoint])
                         }
                deriving (Show, Eq)
$( makeLens ''TrackState )
                
buildTrack :: Int -> Dist -> (Time, Point R2) -> State TrackState ()
buildTrack maxDepth maxDist (t,pt) = do
    trks <- access tracks
    void $ case findTrack maxDepth maxDist trks (t,pt) of
        Nothing  -> do tid <- lastTrack !%%= \x->(x, x+1)
                       tracks !%= M.insert tid [(t,pt)]
        Just tid -> tracks !%= M.update (\trk->Just $ (t,pt):trk) tid

track :: Dist -> V.Vector DataPoint -> Map TrackID (V.Vector DataPoint)
track maxDist pts =
    M.map V.fromList $ tracks ^$ execState f def
    where f = forM_ pts $ buildTrack 100 maxDist
          def = TState (TID 0) M.empty

