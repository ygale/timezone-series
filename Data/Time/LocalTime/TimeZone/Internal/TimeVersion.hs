{-# LANGUAGE CPP #-}

-- | Functions that depend on the version of the @time@ library

module Data.Time.LocalTime.TimeZone.Internal.TimeVersion
(
   mapBuiltTime
 , mapFormatCharacter
)
where

#if MIN_VERSION_time(1,6,0)

mapBuiltTime :: Functor f => (a -> b) -> f a -> f b
mapBuiltTime = fmap

#else

mapBuiltTime :: a -> a
mapBuiltTime = id

#endif

#if MIN_VERSION_time(1,9,1)

mapFormatCharacter :: (a -> b) -> Maybe (x -> b -> String) -> Maybe (x -> a -> String)
mapFormatCharacter f = fmap (\g x -> g x . f)

#elif MIN_VERSION_time(1,8,0)

mapFormatCharacter ::
  (a -> b) -> (c -> d -> e -> b -> z) -> c -> d -> e -> a -> z
mapFormatCharacter f g locale mpado mwidth = g locale mpado mwidth . f

#else

mapFormatCharacter :: (a -> b) -> (c -> d -> b -> z) -> c -> d -> a -> z
mapFormatCharacter f g locale mpado = g locale mpado . f

#endif
