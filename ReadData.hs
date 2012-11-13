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

a .!! b = parseField $! a `V.unsafeIndex` b

instance FromRecord FramePt where
    parseRecord v
        | V.length v == 9  = framePt <$> v .!! 0
                                     <*> v .!! 1
                                     <*> v .!! 2
                                     <*> v .!! 3
                                     <*> v .!! 4
                                     <*> v .!! 5
                                     <*> v .!! 6
                                     <*> v .!! 7
                                     <*> v .!! 8
        | otherwise       = mzero 

{-# INLINABLE parse #-}
parse :: BS.ByteString -> Either String (Vector FramePt)
parse = 
    decodeWith opts . BS.dropWhile (/= fromIntegral (ord '\n'))
    where opts = defaultDecodeOptions { decDelimiter = fromIntegral $ ord '\t' }

{-# INLINABLE readFramePts #-}
readFramePts :: FilePath -> IO (Either String (Vector FramePt))
readFramePts f =
    parse <$> BS.readFile f

