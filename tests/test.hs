{-# LANGUAGE OverloadedStrings #-}

import           Data.Attoparsec.Text
import           NMEA.GPGGA
import           Test.Tasty
import           Test.Tasty.HUnit
import           Data.Time.Calendar (fromGregorian)
import           Data.Time.LocalTime (LocalTime(..), ZonedTime(..), midday, utc, utcToLocalTimeOfDay, zonedTimeZone)

main = defaultMain $ testCase "GPGGA" $ do
    let sGpgga = "$GPGGA,120000.000,4250.5589,S,14718.5084,E,1,04,24.4,19.7,M,,,,0000*1F"
        day = fromGregorian 1970 1 1
        eGpgga = Gpgga {
          _gppgaTimeUTC = ZonedTime (LocalTime day midday) utc
        , _gppgaLatitude = Latitude {_latitudeValue = 4250.5589, _latitudeDirection = South}
        , _gppgaLongitude = Longitude {_longitudeValue = 14718.5084, _longitudeDirection = East},
          _gppgaGpsQuality = GPSFix,
          _gppgaNumberOfSatellites = 4,
          _gppgaHorizDilutionOfPrecision = 24.4,
          _gppgaAltitude = 19.7,
          _gppgaGeoidalSeparation = 0.0,
          _gppgaAgeDifferentialGPSData = 0.0,
          _gppgaDgpsReferenceStation = DGPSReferenceStation {_dgpsReferenceStation = 0}
          }
    (parseOnly gpgga sGpgga) @=? (Right eGpgga)
