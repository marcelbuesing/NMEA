{-# LANGUAGE OverloadedStrings #-}
module NMEA.GPGGA where

import           Control.Applicative ((<|>))
import           Data.Attoparsec.Text
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Data.Time.LocalTime (ZonedTime, utc, zonedTimeZone)
import           Data.Time.Format (defaultTimeLocale, formatTime, parseTimeOrError, readSTime)

-- $GPGGA,HHMMSS.ss,BBBB.BBBB,b,LLLLL.LLLL,l,Q,NN,D.D,H.H,h,G.G,g,A.A,RRRR*PP

-- $GPGGA,235947.000,0000.0000,N,00000.0000,E,0,00,0.0,0.0,M,,,,0000*00
-- $GPGGA,092204.999,4250.5589,S,14718.5084,E,1,04,24.4,19.7,M,,,,0000*1F

data LatitudeDirection = North | South deriving Show

data Latitude = Latitude
  { _latitudeValue     :: Double
  , _latitudeDirection :: LatitudeDirection
  } deriving Show

data LongitudeDirection = East | West deriving Show

data Longitude = Longitude
  { _longitudeValue     :: Double
  , _longitudeDirection :: LongitudeDirection
  } deriving Show

data PositionFixIndicator = InvalidFix | GPSFix | DGPSFix | DeadReckoningMode deriving Show

newtype DGPSReferenceStation = DGPSReferenceStation { _dgpsReferenceStation :: Int } deriving Show

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
  } deriving Show

timeUTC :: Parser ZonedTime
timeUTC = do
  hh <- T.pack <$> count 2 digit
  mm <- T.pack <$> count 2 digit
  ss <- T.pack <$> count 2 digit
  sss  <- T.pack <$> option "" (char '.' *> many1 digit)
  let ztime = ptime (T.unpack (hh <> mm <> ss <> "." <> sss))
  return $ ztime { zonedTimeZone = utc}
  where ptime s = parseTimeOrError False defaultTimeLocale "%H%M%S%Q" s :: ZonedTime

latitudeDirection :: Parser LatitudeDirection
latitudeDirection =
      (char 'N' >> return North)
  <|> (char 'S' >> return South)

latitude :: Parser Latitude
latitude = do
  v <- double
  _ <- char ','
  d <- latitudeDirection
  return (Latitude v d) <?> "Latitude"

longitudeDirection :: Parser LongitudeDirection
longitudeDirection =
      (char 'E' >> return East)
  <|> (char 'W' >> return West)
  <?> "Longitude Direction"

longitude :: Parser Longitude
longitude = do
  v <- double
  _ <- char ','
  d <- longitudeDirection
  return (Longitude v d) <?> "Longitude"

gpsQuality :: Parser PositionFixIndicator
gpsQuality =
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
  _    <- string "$GPGGA,"
  time <- timeUTC
  _    <- char ','
  lat  <- latitude
  _    <- char ','
  lon  <- longitude
  _    <- char ','
  qual <- gpsQuality
  _    <- char ','
  nsat <- decimal :: Parser Int
  _    <- char ','
  dilu <- double
  _    <- char ','
  alti <- double
  _    <- char ',' *> char 'M' *> ","
  geoi <- option 0 double
  _    <- char ',' *> option 'M' (char 'M') *> ","
  age  <- option 0 double
  _    <- char ','
  dgps <- option (DGPSReferenceStation 0) dgpsReferenceStation
  _    <- char '*'
  cs   <- hexadecimal :: Parser Int
  return (Gpgga time lat lon qual nsat dilu alti geoi age dgps) <?> "GPPGA"
