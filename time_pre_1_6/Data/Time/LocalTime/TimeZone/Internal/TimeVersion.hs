-- | Functions that depend on the version of the @time@ library

module Data.Time.LocalTime.TimeZone.Internal.TimeVersion
(
   mapBuiltTime
 , mapFormatCharacter
)
where

-- | No mapping needed for 'buildTime' for pre-1.6 time library.
mapBuiltTime :: a -> a
mapBuiltTime = id

-- | Map for 'formatCharacter' for pre-1.8 time library.
mapFormatCharacter :: (a -> b) -> (c -> d -> b -> z) -> c -> d -> a -> z
mapFormatCharacter f g locale mpado = g locale mpado . f
