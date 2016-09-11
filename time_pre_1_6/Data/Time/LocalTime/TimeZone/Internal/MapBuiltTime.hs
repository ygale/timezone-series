-- | Specify the mapping function for 'buildTime'.

module Data.Time.LocalTime.TimeZone.Internal.MapBuiltTime
(
  mapBuiltTime
)
where

-- | No mapping needed for 'buildTime' for pre-1.6 time library.
mapBuiltTime :: a -> a
mapBuiltTime = id
