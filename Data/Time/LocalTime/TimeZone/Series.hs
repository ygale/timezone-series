-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Time.LocalTime.TimeZone.Series
-- Copyright   :  Yitzchak Gale 2010
--
-- Maintainer  :  Yitzchak Gale <gale@sefer.org>
-- Portability :  portable
--
-- A @TimeZoneSeries@ describes a timezone by specifying the various
-- clock settings that occurred in the past and are scheduled to occur
-- in the future for the timezone.

{- Copyright (c) 2010 Yitzchak Gale. All rights reserved.  For
licensing information, see the BSD3-style license in the file LICENSE
that was originally distributed by the author together with this file.
-}

module Data.Time.LocalTime.TimeZone.Series
(
  -- * Representing a timezone
  -- $abouttzs
  TimeZoneSeries(..),
  timeZoneFromSeries,
  isValidLocalTime,
  isRedundantLocalTime,
  latestNonSummer,

  -- ** Converting between UTC and local time
  -- $aboutfuncs
  utcToLocalTime',
  localTimeToUTC',

  -- * Representing a moment in a timezone
  ZoneSeriesTime(..),
  zonedTimeToZoneSeriesTime,
  zoneSeriesTimeToLocalTime,
  zoneSeriesTimeZone,
  localTimeToZoneSeriesTime
)
where

import Data.Time (UTCTime, LocalTime, TimeZone(timeZoneSummerOnly),
                  ZonedTime(ZonedTime),
                  ParseTime(buildTime), FormatTime(formatCharacter),
                  utcToLocalTime, localTimeToUTC)
import Data.List (partition)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Typeable (mkTyCon, mkTyConApp, Typeable(typeOf))
import Control.Arrow (first)

-- $abouttzs
-- A @TimeZoneSeries@ describes a timezone with a set of @TimeZone@
-- objects. Each @TimeZone@ object describes the clocks in the
-- timezone for a specific period of history during which the clocks
-- do not change.
--
-- Most operating systems provide information about timezone series
-- for the local timezone and for many other timezones of the world.
-- On MS Windows systems, this information can be read from the
-- registry. On other systems, this information is typically provided
-- in the form of Olson timezone files, /etc/localtime (or some other
-- file) for the local timezone and files located in
-- /usr/share/zoneinfo/ or /etc/zoneinfo (or some other directory) for
-- other timezones.

-- | A @TimeZoneSeries@ consists of a default @TimeZone@ object and a
-- sequence of pairs of a @UTCTime@ and a @TimeZone@ object. Each
-- @UTCTime@ indicates a moment at which the clocks changed, and the
-- corresponding @TimeZone@ object describes the new state of the
-- clocks after the change. The default @TimeZone@ object is used for
-- times preceding the earliest @UTCTime@, or if the sequence of pairs
-- is empty. The times in the sequence are in order from latest to
-- earlist (note that this is the opposite of the way that they are
-- stored in an Olson timezone file).
data TimeZoneSeries =
       TimeZoneSeries
         TimeZone
         [(UTCTime, TimeZone)]
                               -- ^ The default timezone state, and a list
                               -- of pairs of the time of a change of clocks
                               -- and the new timezone state after the change
  deriving (Eq, Ord)

instance Typeable TimeZoneSeries where
  typeOf _ = mkTyConApp
    (mkTyCon "Data.Time.LocalTime.TimeZone.Series") []

instance Show TimeZoneSeries where
  show = show . latestNonSummer

instance Read TimeZoneSeries where
    readsPrec n = map (first $ flip TimeZoneSeries []) . readsPrec n

instance ParseTime TimeZoneSeries where
  buildTime locale = flip TimeZoneSeries [] . buildTime locale

-- | The latest non-summer @TimeZone@ in a @TimeZoneSeries@ is in some
-- sense representative of the timezone.
latestNonSummer :: TimeZoneSeries -> TimeZone
latestNonSummer (TimeZoneSeries d cs) = fromMaybe d . listToMaybe $
    filter (not . timeZoneSummerOnly) tzs ++ tzs
  where
    tzs = map snd cs

instance FormatTime TimeZoneSeries where
  formatCharacter = 
    fmap (\f locale mpado -> f locale mpado . latestNonSummer) .
    formatCharacter

-- | Given a timezone represented by a @TimeZoneSeries@, and a @UTCTime@,
-- provide the state of the timezone's clocks at that time.
timeZoneFromSeries :: TimeZoneSeries -> UTCTime -> TimeZone
timeZoneFromSeries (TimeZoneSeries dfault changes) t = fromMaybe dfault .
  fmap snd . listToMaybe . dropWhile ((> t) . fst) $ changes

-- The following functions attempt to deal correctly with corner cases
-- where multiple clock changes overlap with each other, even though
-- as far as I know such weird cases have never actually occurred in
-- any timezone. So far.

-- | When a clock change moves the clock forward, local times that
-- are between the wall clock time before the change and the wall
-- clock time after the change cannot occur.
isValidLocalTime :: TimeZoneSeries -> LocalTime -> Bool
isValidLocalTime tzs lt = gapsVsOverlaps tzs lt /= GT

-- | When a clock change moves the clock backward, local times that
-- are between the wall clock time before the change and the wall
-- clock time after the change occur twice.
isRedundantLocalTime :: TimeZoneSeries -> LocalTime -> Bool
isRedundantLocalTime tzs lt = gapsVsOverlaps tzs lt == LT

gapsVsOverlaps (TimeZoneSeries d cs) lt = compareLengths gaps overlaps
  where
    (gaps, overlaps) = partition (uncurry (<)) relevantIntervals
    relevantIntervals = takeWhile notTooEarly . dropWhile tooLate $
                        gapsAndOverlaps
    tooLate (a, b) = a > lt && b > lt
    notTooEarly (a, b) = a > lt || b > lt
    gapsAndOverlaps = zip
      (zipWith utcToLocalTime (map snd (drop 1 cs) ++ [d]) (map fst cs))
      (map (uncurry $ flip utcToLocalTime) cs)

-- Fast lazy comparison of list lengths
compareLengths :: [a] -> [a] -> Ordering
compareLengths (_:xs) (_:ys) = compareLengths xs ys
compareLengths (_:_ ) _      = GT
compareLengths _      (_:_ ) = LT
compareLengths _      _      = EQ

-- | A @ZoneSeriesTime@ represents a moment of time in the context of
-- a particular timezone.
data ZoneSeriesTime = ZoneSeriesTime {
       zoneSeriesTimeToUTC :: UTCTime,
       zoneSeriesTimeSeries :: TimeZoneSeries
    }
  deriving (Eq, Ord)

instance Typeable ZoneSeriesTime where
  typeOf _ = mkTyConApp
    (mkTyCon "Data.Time.LocalTime.TimeZone.ZoneSeriesTime") []

instance Show ZoneSeriesTime where
  show tzs = show $ ZonedTime (zoneSeriesTimeToLocalTime tzs)
                              (zoneSeriesTimeZone tzs)

instance Read ZoneSeriesTime where
    readsPrec n = map (first zonedTimeToZoneSeriesTime) . readsPrec n

instance ParseTime ZoneSeriesTime where
  buildTime locale = zonedTimeToZoneSeriesTime . buildTime locale

instance FormatTime ZoneSeriesTime where
  formatCharacter =
    fmap (\f locale mpado -> f locale mpado . zoneSeriesTimeToZonedTime) .
    formatCharacter

-- | The @ZonedTime@ of a @ZoneSeriesTime@.
zoneSeriesTimeToZonedTime :: ZoneSeriesTime -> ZonedTime
zoneSeriesTimeToZonedTime zst =
  ZonedTime (zoneSeriesTimeToLocalTime zst) (zoneSeriesTimeZone zst)

-- | Use a trivial @TimeZoneSeries@ containing only the @TimeZone@
-- of the @ZonedTime@, and use it to define a @ZoneSeriesTime@.
zonedTimeToZoneSeriesTime :: ZonedTime -> ZoneSeriesTime
zonedTimeToZoneSeriesTime (ZonedTime t tz) =
  ZoneSeriesTime (localTimeToUTC tz t) (TimeZoneSeries tz [])

-- | The local time represented by a @ZoneSeriesTime@
zoneSeriesTimeToLocalTime :: ZoneSeriesTime -> LocalTime
zoneSeriesTimeToLocalTime (ZoneSeriesTime t tzs) = utcToLocalTime' tzs t

-- | The @TimeZone@ that is in effect at the moment represented by
-- a @ZoneSeriesTime@.
zoneSeriesTimeZone :: ZoneSeriesTime -> TimeZone
zoneSeriesTimeZone (ZoneSeriesTime t tzs) = timeZoneFromSeries tzs t

-- | The @ZoneSeriesTime@ that represents the given local time in the
-- given timezone. Local times that are invalid or redundant are treated
-- as described below.
localTimeToZoneSeriesTime :: TimeZoneSeries -> LocalTime -> ZoneSeriesTime
localTimeToZoneSeriesTime tzs lt = ZoneSeriesTime (localTimeToUTC' tzs lt) tzs

-- $aboutfuncs
-- The following functions are variants on functions in "Data.Time.LocalTime"
-- that convert between UTC and local time. The originals can give the
-- wrong result if the 'TimeZone' used for the conversion is not actually
-- in effect at the specified time. These variants use a 'TimeZoneSeries'
-- instead of a 'TimeZone', so they are always correct.
--
-- When converting from an invalid local time, the local time is interpreted
-- as if the time change that made it invalid never happened.
-- When converting from a redundant local time, the latest possible
-- interpretation is used. Use the functions 'isValidLocalTime' and
-- 'isRedundantLocalTime' to detect these conditions.

-- | Convert a UTC time to local time using the "TimeZone" that is in
-- effect at that time in the timezone represented by TimeZoneSeries.
utcToLocalTime' :: TimeZoneSeries -> UTCTime -> LocalTime
utcToLocalTime' tzs utc = utcToLocalTime (timeZoneFromSeries tzs utc) utc

-- | Convert a local time to UTC using the "TimeZone" that is in
-- effect at that time in the timezone represented by TimeZoneSeries.
-- Local times that are invalid or redundant are treated as described above.
localTimeToUTC' :: TimeZoneSeries -> LocalTime -> UTCTime
localTimeToUTC' (TimeZoneSeries dfault changes) lt =
  fromMaybe (localTimeToUTC dfault lt) . fmap snd . listToMaybe .
  dropWhile (uncurry (>)) $
  zip (map fst changes) (map (flip localTimeToUTC lt . snd) changes)
