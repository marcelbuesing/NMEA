{-# LANGUAGE OverloadedStrings #-}

module NMEA.GPRMC where

import           Control.Applicative ((<|>))
import           Data.Attoparsec.Text
import           Data.Time.Calendar (Day(..))
import           Data.Time.LocalTime (ZonedTime(..))
import           NMEA.Common

data GprmcStatus = DataValid | DataNotValid deriving (Eq, Show)

data GprmcMode = Autonomous | DGPS | DR deriving (Eq, Show)

data MagneticVariation = MagneticVariation
  { _magneticVariationValue     :: Degree
  , _magneticVariationDirection :: LongitudeDirection
  } deriving (Eq, Show)

-- | Recommended minimum specific GNSS data
data Gprmc = Gprmc
  { _gprmcTimeUTC           :: ZonedTime
  , _gprmcStatus            :: GprmcStatus
  , _gprmcLatitude          :: Latitude
  , _gprmcLongitude         :: Longitude
  , _gprmcSpeedOverGround   :: Knot
  , _gprmcCourseOverGround  :: Degree
  , _gpmrcDate              :: Day
  , _gpmrcMagneticVariation :: MagneticVariation
  , _gprmcMode              :: GprmcMode
  } deriving (Eq, Show)

gprmcStatus :: Parser GprmcStatus
gprmcStatus =
      (char 'A' >> return DataValid)
  <|> (char 'V' >> return DataNotValid)

magneticVariation :: Parser MagneticVariation
magneticVariation = do
  v <- degree
  _ <- comma
  d <- longitudeDirection
  return (MagneticVariation v d) <?> "Magnetic Variation"

gprmcMode :: Parser GprmcMode
gprmcMode =
      (char 'A' >> return Autonomous)
  <|> (char 'D' >> return DGPS)
  <|> (char 'E' >> return DR)

gprmc :: Century -> Parser Gprmc
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
