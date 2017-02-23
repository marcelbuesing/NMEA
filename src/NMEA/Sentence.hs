{-# LANGUAGE OverloadedStrings #-}
module NMEA.Sentence where

import           Control.Applicative ((<|>))
import           Data.Attoparsec.Text
import           Data.Time.Calendar (Day(..))
import           Data.Time.LocalTime (ZonedTime(..))

import NMEA.Common
import NMEA.GPGGA
import NMEA.GPRMC

data Sentence =
    -- | Recommended minimum specific GNSS data
  Gprmc
  { _gprmcTimeUTC           :: ZonedTime
  , _gprmcStatus            :: GprmcStatus
  , _gprmcLatitude          :: Latitude
  , _gprmcLongitude         :: Longitude
  , _gprmcSpeedOverGround   :: Knot
  , _gprmcCourseOverGround  :: Degree
  , _gpmrcDate              :: Day
  , _gpmrcMagneticVariation :: MagneticVariation
  , _gprmcMode              :: GprmcMode
  } |
  -- | Global Positioning System Fix Data
  Gpgga
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
  } |
  -- | Heading from True North
  Gphdt
  { _gphdtHeadingInDegrees :: Degree
  }
  deriving (Eq, Show)

sentence :: Century -> Parser Sentence
sentence century = gpgga <|> gprmc century

gprmc :: Century -> Parser Sentence
gprmc cen = do
  _       <- string "$GPRMC,"
  time <- timeUTC
  _    <- comma
  stat <- gprmcStatus
  _    <- comma
  lat  <- latitude
  _    <- comma
  lon  <- longitude
  _    <- comma
  spd  <- knot
  _    <- comma
  deg  <- degree
  _    <- comma
  date <- day cen
  _    <- comma
  mv   <- option (MagneticVariation (Degree 0) East) magneticVariation
--  _    <- comma
--  mode <- option Autonomous gprmcMode
  _    <- comma
  _    <- checksum
  return $ Gprmc time stat lat lon spd deg date mv Autonomous

gpgga :: Parser Sentence
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

gphdt = do
  string "$GPHDT"
  _    <- comma
  deg  <- degree
  _    <- comma
  _    <- char 'T'
  _    <- checksum
  return $ Gphdt deg
