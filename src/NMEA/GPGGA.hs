{-# LANGUAGE OverloadedStrings #-}

module NMEA.GPGGA where

import           Control.Applicative ((<|>))
import           Data.Attoparsec.ByteString.Char8
import           Data.Monoid ((<>))
import qualified Data.Text as T

import NMEA.Common

-- $GPGGA,HHMMSS.ss,BBBB.BBBB,b,LLLLL.LLLL,l,Q,NN,D.D,H.H,h,G.G,g,A.A,RRRR*PP

-- $GPGGA,235947.000,0000.0000,N,00000.0000,E,0,00,0.0,0.0,M,,,,0000*00
-- $GPGGA,092204.999,4250.5589,S,14718.5084,E,1,04,24.4,19.7,M,,,,0000*1F

-- | GPS quality indicator
data PositionFixIndicator =
    InvalidFix
  | GPSFix
  | DGPSFix
  | PPSFix
  | RTKFix
  | FRTKFix
  | DeadReckoningMode
  | ManualMode
  | SimulationMode
  deriving (Eq, Show)

newtype DGPSReferenceStation = DGPSReferenceStation { _dgpsReferenceStation :: Int } deriving (Eq, Show)

positionFixIndicator :: Parser PositionFixIndicator
positionFixIndicator =
      (char '0' >> return InvalidFix)
  <|> (char '1' >> return GPSFix)
  <|> (char '2' >> return DGPSFix)
  <|> (char '3' >> return PPSFix)
  <|> (char '4' >> return RTKFix)
  <|> (char '5' >> return FRTKFix)
  <|> (char '6' >> return DeadReckoningMode)
  <|> (char '7' >> return ManualMode)
  <|> (char '8' >> return SimulationMode)
  <?> "GPS Quality"

numberOfSatellites :: Parser Int
numberOfSatellites = read <$> count 2 digit <?> "Number of satellites"

dgpsReferenceStation :: Parser DGPSReferenceStation
dgpsReferenceStation = do
  d <- read <$> count 4 digit
  return (DGPSReferenceStation d) <?> "DGPSReferenceStation"
