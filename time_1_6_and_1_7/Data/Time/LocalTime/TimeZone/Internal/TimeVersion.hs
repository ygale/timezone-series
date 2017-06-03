-- | Functions that depend on the version of the @time@ library

module Data.Time.LocalTime.TimeZone.Internal.TimeVersion
(
   mapBuiltTime
 , mapFormatCharacter
)
where

-- | Map over 'Maybe' for 'buildTime' for post-1.6 time library.
mapBuiltTime :: Functor f => (a -> b) -> f a -> f b
mapBuiltTime = fmap

-- | Map for 'formatCharacter' for pre-1.8 time library.
mapFormatCharacter :: (a -> b) -> (c -> d -> b -> z) -> c -> d -> a -> z
mapFormatCharacter f g locale mpado = g locale mpado . f
