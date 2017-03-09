{-# LANGUAGE OverloadedStrings #-}
module NMEA.Sentence where

import           Control.Applicative ((<|>))
import           Data.Attoparsec.ByteString.Char8
import           Data.Time.Calendar (Day(..))
import           Data.Time.LocalTime (ZonedTime(..))
import           Data.Maybe (catMaybes)

import NMEA.Common
import NMEA.GPGGA
import NMEA.GPRMC
import NMEA.GPGSA
import NMEA.GPGSV

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
  , _gpmrcMagneticVariation :: Maybe MagneticVariation
  , _gprmcMode              :: GprmcMode
  } |
  -- | Global Positioning System Fix Data
  Gpgga
  { _gpggaTimeUTC                  :: ZonedTime
  , _gpggaLatitude                 :: Latitude
  , _gpggaLongitude                :: Longitude
  , _gpggaGpsQuality               :: PositionFixIndicator
  , _gpggaNumberOfSatellites       :: Int
  , _gpggaHorizDilutionOfPrecision :: Double
  , _gpggaAltitude                 :: Double
  , _gpggaGeoidalSeparation        :: Double
  , _gpggaAgeDifferentialGPSData   :: Double
  , _gpggaDgpsReferenceStation     :: DGPSReferenceStation
  } |
  -- | Heading from True North
  Gphdt
  { _gphdtHeadingInDegrees :: Degree
  } |
  -- | GPS DOP and active satellites
  Gpgsa
  { _gpgsaMode          :: GPGSAMode
  , _gpgsaPositionFix   :: PositionFix
  , _gpgsaSatellitenPRN :: [SatellitePRN]
  , _gpgsaPDOP          :: PDOP
  , _gpgsaHDOP          :: HDOP
  , _gpgsaVDOP          :: VDOP
  } |
  -- | GPS satellites in view
  Gpgsv
  {
    _gpgsvTotalMessage :: Int
  , _gpgsvMessageNumber :: Int
  , _gpgsvNumberOfSatellitesInView :: Int
  , _gpgsvSatellitesInView :: [SatelliteInView]
  }
  deriving (Eq, Show)

sentence :: Century -> Parser Sentence
sentence century =
      gpgga
  <|> gprmc century
  <|> gphdt
  <|> gpgsa
  <|> gpgsv

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
  mv   <- magneticVariation
  _    <- comma
  mode <- gprmcMode
  _    <- checksum
  return $ Gprmc time stat lat lon spd deg date mv mode

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

gphdt :: Parser Sentence
gphdt = do
  string "$GPHDT"
  _    <- comma
  deg  <- degree
  _    <- comma
  _    <- char 'T'
  _    <- checksum
  return $ Gphdt deg

gpgsa :: Parser Sentence
gpgsa = do
  string "$GPGSA"
  _     <- comma
  mode  <- gpgsaMode
  _     <- comma
  fix   <- positionFix
  _     <- comma
  sats  <- catMaybes <$> count 12 ((option Nothing $ Just <$> satellitePRN) <* comma)
  pdop' <- pdop
  _     <- comma
  hdop' <- hdop
  _     <- comma
  vdop' <- vdop
  _     <- checksum
  return $ Gpgsa mode fix  sats pdop' hdop' vdop'

gpgsv :: Parser Sentence
gpgsv = do
  string "$GPGSV"
  _ <- comma
  totalMsg  <- decimal
  _         <- comma
  msgNumber <- decimal
  _         <- comma
  inView    <- decimal
  sats      <- catMaybes <$> count 4 (comma *> satelliteInView)
  --_         <- checksum
  return $ Gpgsv totalMsg msgNumber inView sats
  
