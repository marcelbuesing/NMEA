{-# LANGUAGE OverloadedStrings #-}

import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString.Char8 (pack)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Data.Time.Calendar (fromGregorian)
import           Data.Time.LocalTime (LocalTime(..), ZonedTime(..), midday, midnight, utc, utcToLocalTimeOfDay, zonedTimeZone)

import           NMEA.Common
import           NMEA.GPGGA
import           NMEA.GPRMC
import           NMEA.GPGSA
import           NMEA.GPGSV
import           NMEA.Sentence

main = defaultMain $ testGroup "NMEA"
  [ testCase "GPGGA" gpggaTest
  , testCase "GPRMC" gprmcTest
  , testCase "GPHDT" gphdtTest
  , testCase "GPGSA" gpgsaTest
  , testCase "GPGSV" gpgsvTest
  , testCase "GPVTG" gpvtgTest
  ]


gpggaTest :: Assertion
gpggaTest = do
    let sGpgga = "$GPGGA,120000.000,4250.5589,S,14718.5084,E,1,04,24.4,19.7,M,,,,0000*1F"
        day' = fromGregorian 1970 1 1
        eGpgga = Gpgga {
          _gpggaTimeUTC = ZonedTime (LocalTime day' midday) utc
        , _gpggaLatitude = Latitude
          { _latitudeValue = toDegreesMinutes 4250.5589
          , _latitudeDirection = South
          }
        , _gpggaLongitude = Longitude
          { _longitudeValue = toDegreesMinutes 14718.5084
          , _longitudeDirection = East
          },
          _gpggaGpsQuality = GPSFix,
          _gpggaNumberOfSatellites = 4,
          _gpggaHorizDilutionOfPrecision = 24.4,
          _gpggaAltitude = 19.7,
          _gpggaGeoidalSeparation = 0.0,
          _gpggaAgeDifferentialGPSData = 0.0,
          _gpggaDgpsReferenceStation = DGPSReferenceStation {_dgpsReferenceStation = 0}
          }
    parseOnly gpgga sGpgga @?= Right eGpgga

gprmcTest :: Assertion
gprmcTest = do
    let sGprmc = "$GPRMC,000000.000,A,3723.2475,N,12158.3416,W,0.13,309.62,120598,0.4,E,A*57"
        day' = fromGregorian 1970 1 1
        magVariation = Just $ MagneticVariation
          { _magneticVariationValue = Degree {_unDegree = 0.4}
          , _magneticVariationDirection = East
          }
        eGprmc = Gprmc
          { _gprmcTimeUTC = ZonedTime (LocalTime day' midnight) utc
          , _gprmcStatus = DataValid
          , _gprmcLatitude = Latitude
            { _latitudeValue = toDegreesMinutes 3723.2475
            , _latitudeDirection = North
            }
          , _gprmcLongitude = Longitude
            { _longitudeValue = toDegreesMinutes 12158.3416
            , _longitudeDirection = West
            }
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

gpgsvTest :: Assertion
gpgsvTest =
  parseOnly gpgsv input @?= Right expected
  where input = "$GPGSV,3,3,11,22,42,067,42,24,14,311,43,27,05,244,00,,,,*4D"
        satA = SatelliteInView (SatellitePRN 22) (Elevation 42) (Azimuth 67) (SatelliteSNR 42)
        satB = SatelliteInView (SatellitePRN 24) (Elevation 14) (Azimuth 311) (SatelliteSNR 43)
        satC = SatelliteInView (SatellitePRN 27) (Elevation 5) (Azimuth 244) (SatelliteSNR 0)
        sats = [satA, satB, satC]
        expected = Gpgsv 3 3 11 sats

gpvtgTest :: Assertion
gpvtgTest =
  parseOnly gpvtg input @?= Right expected
  where input = "$GPVTG,0.0,T,359.6,M,0.0,N,0.0,K*47"
        true = Just $ TrueBearing $ toDegreesMinutes 0.0
        magn = Just $ MagneticBearing $ toDegreesMinutes 359.6
        knot' = Knot 0.0
        kmh' = Kmh 0.0
        expected = Gpvtg true magn knot' kmh'
