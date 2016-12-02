{-# LANGUAGE OverloadedStrings #-}

module NMEA.GPGGA where

import           Control.Applicative ((<|>))
import           Data.Attoparsec.Text
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Data.Time.LocalTime (ZonedTime(..))

import NMEA.Common

-- $GPGGA,HHMMSS.ss,BBBB.BBBB,b,LLLLL.LLLL,l,Q,NN,D.D,H.H,h,G.G,g,A.A,RRRR*PP

-- $GPGGA,235947.000,0000.0000,N,00000.0000,E,0,00,0.0,0.0,M,,,,0000*00
-- $GPGGA,092204.999,4250.5589,S,14718.5084,E,1,04,24.4,19.7,M,,,,0000*1F

-- | GPS quality indicator
data PositionFixIndicator =
    InvalidFix
  | GPSFix
  | DGPSFix
  | DeadReckoningMode deriving (Eq, Show)

newtype DGPSReferenceStation = DGPSReferenceStation { _dgpsReferenceStation :: Int } deriving (Eq, Show)

-- | Global Positioning System Fix Data
data Gpgga = Gpgga
  { _gppgaTimeUTC                  :: ZonedTime
  , _gppgaLatitude                 :: Latitude
  , _gppgaLongitude                :: Longitude
  , _gppgaGpsQuality               :: PositionFixIndicator
  , _gppgaNumberOfSatellites       :: Int
  , _gppgaHorizDilutionOfPrecision :: Double
  , _gppgaAltitude                 :: Double
  , _gppgaGeoidalSeparation        :: Double
  , _gppgaAgeDifferentialGPSData   :: Double
  , _gppgaDgpsReferenceStation     :: DGPSReferenceStation
  } deriving (Eq, Show)

positionFixIndicator :: Parser PositionFixIndicator
positionFixIndicator =
      (char '0' >> return InvalidFix)
  <|> (char '1' >> return GPSFix)
  <|> (char '2' >> return DGPSFix)
  <|> (char '6' >> return DeadReckoningMode)
  <?> "GPS Quality"

numberOfSatellites :: Parser Int
numberOfSatellites = read <$> count 2 digit <?> "Number of satellites"

dgpsReferenceStation :: Parser DGPSReferenceStation
dgpsReferenceStation = do
  d <- read <$> count 4 digit
  return (DGPSReferenceStation d) <?> "DGPSReferenceStation"

gpgga :: Parser Gpgga
gpgga = do
  string "$GPGGA,"
  time <- timeUTC
  _    <- comma
  lat  <- latitude
  _    <- comma
  lon  <- longitude
  _    <- comma
  qual <- positionFixIndicator
  _    <- comma
  nsat <- decimal :: Parser Int
  _    <- comma
  dilu <- double
  _    <- comma
  alti <- double
  _    <- comma *> char 'M' *> ","
  geoi <- option 0 double
  _    <- comma *> option 'M' (char 'M') *> comma
  age  <- option 0 double
  _    <- comma
  dgps <- option (DGPSReferenceStation 0) dgpsReferenceStation
  _    <- checksum
  return (Gpgga time lat lon qual nsat dilu alti geoi age dgps) <?> "GPPGA"
