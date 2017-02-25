{-# LANGUAGE OverloadedStrings #-}

module NMEA.GPRMC where

import           Control.Applicative ((<|>))
import           Data.Attoparsec.ByteString.Char8
import           Data.Time.LocalTime (ZonedTime(..))
import           NMEA.Common

data GprmcStatus = DataValid | DataNotValid deriving (Eq, Show)

data GprmcMode = Autonomous | DGPS | DR deriving (Eq, Show)

data MagneticVariation = MagneticVariation
  { _magneticVariationValue     :: Degree
  , _magneticVariationDirection :: LongitudeDirection
  } deriving (Eq, Show)


gprmcStatus :: Parser GprmcStatus
gprmcStatus =
      (char 'A' >> return DataValid)
  <|> (char 'V' >> return DataNotValid)

magneticVariation :: Parser (Maybe MagneticVariation)
magneticVariation = do
  v <- option Nothing $ Just <$> degree
  _ <- comma
  d <- option Nothing $ Just <$> longitudeDirection
  return $ MagneticVariation <$> v <*> d

gprmcMode :: Parser GprmcMode
gprmcMode =
      (char 'A' >> return Autonomous)
  <|> (char 'D' >> return DGPS)
  <|> (char 'E' >> return DR)
