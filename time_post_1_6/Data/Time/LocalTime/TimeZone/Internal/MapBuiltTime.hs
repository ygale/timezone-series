-- | Specify the mapping function for 'buildTime'.

module Data.Time.LocalTime.TimeZone.Internal.MapBuiltTime
(
  mapBuiltTime
)
where

-- | Map over 'Maybe' for 'buildTime' for post-1.6 time library.
mapBuiltTime :: Functor f => (a -> b) -> f a -> f b
mapBuiltTime = fmap
