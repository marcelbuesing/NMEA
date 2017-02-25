{-# LANGUAGE OverloadedStrings #-}

module NMEA.GPGSV where

import           Data.Attoparsec.Text

import NMEA.Common

data SatelliteInView = SatelliteInView
  { _satelliteInViewPRN       :: SatellitePRN
  , _satelliteInViewElevation :: Elevation
  , _satelliteInViewAzimuth   :: Azimuth
  , _satelliteInViewSNR       :: SatelliteSNR
  } deriving (Eq, Show)

satelliteInView :: Parser (Maybe SatelliteInView)
satelliteInView = do
  prn  <- option Nothing $ Just  <$> satellitePRN
  _ <- comma
  elev <- option Nothing $ Just  <$> elevation
  _ <- comma
  azi  <- option Nothing $ Just  <$> azimuth
  _ <- comma
  snr  <- option Nothing $ Just  <$>  satelliteSNR
  return $ SatelliteInView <$> prn <*> elev <*> azi <*> snr
