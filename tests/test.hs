{-# LANGUAGE OverloadedStrings #-}

import           Data.Attoparsec.Text
import           Test.Tasty
import           Test.Tasty.HUnit
import           Data.Time.Calendar (fromGregorian)
import           Data.Time.LocalTime (LocalTime(..), ZonedTime(..), midday, midnight, utc, utcToLocalTimeOfDay, zonedTimeZone)

import           NMEA.Common
import           NMEA.GPGGA
import           NMEA.GPRMC
import           NMEA.GPGSA
import           NMEA.Sentence

main = defaultMain $ testGroup "NMEA"
  [ testCase "GPGGA" gpggaTest
  , testCase "GPRMC" gprmcTest
  , testCase "GPHDT" gphdtTest
  , testCase "GPGSA" gpgsaTest
  ]


gpggaTest :: Assertion
gpggaTest = do
    let sGpgga = "$GPGGA,120000.000,4250.5589,S,14718.5084,E,1,04,24.4,19.7,M,,,,0000*1F"
        day' = fromGregorian 1970 1 1
        eGpgga = Gpgga {
          _gppgaTimeUTC = ZonedTime (LocalTime day' midday) utc
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
    parseOnly gpgga sGpgga @?= Right eGpgga

gprmcTest :: Assertion
gprmcTest = do
    let sGprmc = "$GPRMC,000000.000,A,3723.2475,N,12158.3416,W,0.13,309.62,120598,,*10"
        day' = fromGregorian 1970 1 1
        magVariation = MagneticVariation
          { _magneticVariationValue = Degree {_unDegree = 0.0}
          , _magneticVariationDirection = East
          }
        eGprmc = Gprmc
          { _gprmcTimeUTC = ZonedTime (LocalTime day' midnight) utc
          , _gprmcStatus = DataValid
          , _gprmcLatitude = Latitude {_latitudeValue = 3723.2475, _latitudeDirection = North}
          , _gprmcLongitude = Longitude {_longitudeValue = 12158.3416, _longitudeDirection = West}
          , _gprmcSpeedOverGround = Knot {_unKnot = 0.13}
          , _gprmcCourseOverGround = Degree {_unDegree = 309.62}
          , _gpmrcDate = fromGregorian 1998 5 12
          , _gpmrcMagneticVariation = magVariation
          , _gprmcMode = Autonomous
          }
    parseOnly (gprmc 1900) sGprmc @?= Right eGprmc

gphdtTest :: Assertion
gphdtTest =
  parseOnly gphdt sGphdt @?= Right eGphdt
  where sGphdt = "$GPHDT,175.58,T*0C"
        eGphdt = Gphdt (Degree 175.58)

gpgsaTest :: Assertion
gpgsaTest =
  parseOnly gpgsa sGpgsa @?= Right eGpgsa
  where sGpgsa = "$GPGSA,A,3,19,28,14,18,27,22,31,39,,,,,1.7,1.0,1.3*35"
        eSats = SatellitePRN <$> [19, 28, 14, 18, 27, 22, 31, 39]
        eGpgsa = Gpgsa Automatic FixNotAvailable eSats (PDOP 1.7) (HDOP 1.0) (VDOP 1.3)
