{-# LANGUAGE OverloadedStrings #-}

module NMEA.GPGSA where

import           Control.Applicative ((<|>))
import           Data.Attoparsec.Text

import NMEA.Common

data GPGSAMode = Manual | Automatic deriving (Eq, Show)

data PositionFix = Fix3D | Fix2D | FixNotAvailable deriving (Eq, Show)

-- | accuracy
newtype PDOP = PDOP { _unPDOP :: Double } deriving (Eq, Show)
-- | horizontal accuracy
newtype HDOP = HDOP { _unHDOP :: Double } deriving (Eq, Show)
-- | vertical accuracy
newtype VDOP = VDOP { _unVDOP :: Double } deriving (Eq, Show)

gpgsaMode :: Parser GPGSAMode
gpgsaMode = do
      (char 'M' >> return Manual)
  <|> (char 'A' >> return Automatic)


positionFix :: Parser PositionFix
positionFix = do
      (char '1' >> return Fix3D)
  <|> (char '2' >> return Fix2D)
  <|> (char '3' >> return FixNotAvailable)

pdop :: Parser PDOP
pdop = PDOP <$> double

hdop :: Parser HDOP
hdop = HDOP <$> double

vdop :: Parser VDOP
vdop = VDOP <$> double
