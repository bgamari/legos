{-# LANGUAGE TupleSections, TemplateHaskell,
             GeneralizedNewtypeDeriving, TypeFamilies #-}

module Track ( track
             , R2(..), Time, Dist, DataPoint
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
import           Control.Lens

data R2 = R2 {-# UNPACK #-} !Double {-# UNPACK #-} !Double
        deriving (Show, Eq)
     
instance AdditiveGroup R2 where
    zeroV = R2 0 0
    R2 x y ^+^ R2 x' y' = R2 (x+x') (y+y')
    negateV (R2 x y) = R2 (-x) (-y)

instance VectorSpace R2 where
    type Scalar R2 = Double
    a *^ R2 x y = R2 (a*x) (a*y)

instance InnerSpace R2 where
    R2 x y <.> R2 x' y' = x*x' + y*y'

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
makeLenses ''TrackState
                
buildTrack :: Int -> Dist -> (Time, Point R2) -> State TrackState ()
buildTrack maxDepth maxDist (t,pt) = do
    trks <- use tracks
    void $ case findTrack maxDepth maxDist trks (t,pt) of
        Nothing  -> do tid <- lastTrack %%= \x->(x, x+1)
                       tracks %= M.insert tid [(t,pt)]
        Just tid -> tracks %= M.update (\trk->Just $ (t,pt):trk) tid

track :: Dist -> V.Vector DataPoint -> Map TrackID (V.Vector DataPoint)
track maxDist pts =
    M.map V.fromList $ tracks ^$ execState f def
    where f = forM_ pts $ buildTrack 100 maxDist
          def = TState (TID 0) M.empty

