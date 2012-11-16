{-# LANGUAGE BangPatterns #-}

module ReadData ( readFramePts
                , FramePt(..)
                ) where 
                
import           Control.Applicative
import           Control.Monad (mzero)

import qualified Data.Vector as V                
import           Data.Vector (Vector)

import           Data.VectorSpace       
import           Data.AffineSpace.Point

import qualified Data.ByteString.Lazy as BS
import           Data.Csv                
import           Data.Char                 

import           Track
                
data FramePt = FramePt { fpId       :: {-# UNPACK #-} !Int
                       , fpArea     :: {-# UNPACK #-} !Int
                       , fpPos      :: {-# UNPACK #-} !(Point R2)
                       , fpCirc     :: {-# UNPACK #-} !Double
                       , fpFrame    :: {-# UNPACK #-} !Int
                       , fpAR       :: {-# UNPACK #-} !Double
                       , fpRound    :: {-# UNPACK #-} !Double
                       , fpSolidity :: {-# UNPACK #-} !Double
                       }
             deriving (Show)
     
framePt !id !area !x !y !circ !frame !ar !round !solidity =
    FramePt id area (P (x,y)) circ frame ar round solidity

a `index` b = parseField $! a `V.unsafeIndex` b

instance FromRecord FramePt where
    parseRecord v
        | V.length v == 9  = framePt <$> v `index` 0
                                     <*> v `index` 1
                                     <*> v `index` 2
                                     <*> v `index` 3
                                     <*> v `index` 4
                                     <*> v `index` 5
                                     <*> v `index` 6
                                     <*> v `index` 7
                                     <*> v `index` 8
        | otherwise       = mzero 

{-# INLINABLE parse #-}
parse :: BS.ByteString -> Either String (Vector FramePt)
parse = 
    decodeWith opts True
    where opts = defaultDecodeOptions { decDelimiter = fromIntegral $ ord '\t' }

{-# INLINABLE readFramePts #-}
readFramePts :: FilePath -> IO (Either String (Vector FramePt))
readFramePts f =
    parse <$> BS.readFile f

