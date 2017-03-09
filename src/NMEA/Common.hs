{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module NMEA.Common where

import           Control.Applicative ((<|>))
import           Data.Attoparsec.ByteString.Char8
import           Data.Time.Calendar (Day(..), fromGregorian)
import           Data.Time.LocalTime (ZonedTime(..), utc, zonedTimeZone)
import           Data.Time.Format (defaultTimeLocale, formatTime, parseTimeOrError, readSTime)
import           Data.Monoid ((<>))
import qualified Data.Text as T

data DegreesMinutes = DegreesMinutes Int Double
  deriving (Eq, Show)


data LatitudeDirection = North | South deriving (Eq, Show)

data Latitude = Latitude
  { _latitudeValue     :: DegreesMinutes
  , _latitudeDirection :: LatitudeDirection
  } deriving (Eq, Show)

data LongitudeDirection = East | West deriving (Eq, Show)

data Longitude = Longitude
  { _longitudeValue     :: DegreesMinutes
  , _longitudeDirection :: LongitudeDirection
  } deriving (Eq, Show)


class HasDD x where
  toDD :: x -> Double


instance HasDD Longitude where
  toDD (Longitude dm East) = toDecimalDegrees dm
  toDD (Longitude dm West) = negate (toDecimalDegrees dm)

instance HasDD Latitude where
  toDD (Latitude dm North) = toDecimalDegrees dm
  toDD (Latitude dm South) = negate (toDecimalDegrees dm)


-- | Speed in knots, equal to one nautical mile per hour.
newtype Knot  = Knot { _unKnot :: Double } deriving (Eq, Show)

-- | e.g. 90°, max 360°
newtype Degree = Degree { _unDegree :: Double } deriving (Eq, Show)

-- | elevation in degree, max 90°
newtype Elevation = Elevation { _unElevation :: Int } deriving (Eq, Show)

-- | Azimuth, degress from true north, 000 to 359
newtype Azimuth = Azimuth { _unAzimuth :: Int } deriving (Eq, Show)

-- | Satellite PRN (pseudo random code), unique satellite id
newtype SatellitePRN = SatellitePRN { _unSatellitePRN :: Int } deriving (Eq, Show)

-- | Signal to noise ratio, 00 - 99 dB
newtype SatelliteSNR = SatelliteSNR { _unSatelliteSNR :: Int } deriving (Eq, Show)

-- | time as UTC missing date information
timeUTC :: Parser ZonedTime
timeUTC = do
  hh <- T.pack <$> count 2 digit
  mm <- T.pack <$> count 2 digit
  ss <- T.pack <$> count 2 digit
  sss  <- T.pack <$> option "" (char '.' *> many1 digit)
  let ztime = ptime (T.unpack (hh <> mm <> ss <> "." <> sss))
  return $ ztime { zonedTimeZone = utc}
  where ptime s = parseTimeOrError False defaultTimeLocale "%H%M%S%Q" s :: ZonedTime

-- e.g. 2000
type Century = Integer

-- | requires current century to determine correct date e.g. 2000
day :: Century -> Parser Day
day century = do
  dd <- read <$> count 2 digit
  mm <- read <$> count 2 digit
  yy <- read <$> count 2 digit
  return $ fromGregorian (century + yy) mm dd

deriving instance Eq ZonedTime

latitudeDirection :: Parser LatitudeDirection
latitudeDirection =
      (char 'N' >> return North)
  <|> (char 'S' >> return South)

latitude :: Parser Latitude
latitude = do
  v <- degreesMinutes
  _ <- comma
  d <- latitudeDirection
  return (Latitude v d) <?> "Latitude"

longitudeDirection :: Parser LongitudeDirection
longitudeDirection =
      (char 'E' >> return East)
  <|> (char 'W' >> return West)
  <?> "Longitude Direction"

longitude :: Parser Longitude
longitude = do
  v <- degreesMinutes
  _ <- comma
  d <- longitudeDirection
  return (Longitude v d) <?> "Longitude"

knot :: Parser Knot
knot = Knot <$> double

degree :: Parser Degree
degree = Degree <$> double

checksum :: Parser Int
checksum =  char '*' *> hexadecimal :: Parser Int

comma :: Parser Char
comma = char ','

satellitePRN :: Parser SatellitePRN
satellitePRN = SatellitePRN <$> decimal

satelliteSNR :: Parser SatelliteSNR
satelliteSNR = SatelliteSNR <$> decimal

elevation :: Parser Elevation
elevation = Elevation <$> decimal

azimuth :: Parser Azimuth
azimuth = Azimuth <$> decimal

degreesMinutes :: Parser DegreesMinutes
degreesMinutes = toDegreesMinutes <$> double


toDegreesMinutes :: Double -> DegreesMinutes
toDegreesMinutes v = DegreesMinutes degrees (v - (fromIntegral (100 * degrees)))
  where
    degrees = floor (v / 100)

toDecimalDegrees :: DegreesMinutes -> Double
toDecimalDegrees (DegreesMinutes d m) = (fromIntegral d) + (m / 60)


