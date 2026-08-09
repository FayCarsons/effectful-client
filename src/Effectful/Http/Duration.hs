{- | Microsecond durations, the unit shared by the backoff algebra in
"Effectful.Http.Retry", the request timeout in "Effectful.Http",
and 'Effectful.Concurrent.threadDelay'.

A 'Duration' is never negative and never wraps: the smart constructors clamp
at zero and every operation saturates at 'Int64' microseconds — around
292,000 years, which the doubling in 'Effectful.Http.Retry.exponential'
would otherwise reach in about sixty attempts.
-}
module Effectful.Http.Duration (
  Duration,
  microseconds,
  milliseconds,
  seconds,
  toMicroseconds,
  fromNominalDiffTime,
  toNominalDiffTime,
  scale,
  delay,
) where

import Data.Fixed           (Pico)
import Data.Int             (Int64)
import Data.Time.Clock      (NominalDiffTime, nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import Effectful            (Eff, (:>))
import Effectful.Concurrent (Concurrent, threadDelay)

newtype Duration = Microseconds Int64
  deriving stock (Eq, Ord)

instance Show Duration where
  show (Microseconds n) = show n <> "µs"

instance Semigroup Duration where
  Microseconds a <> Microseconds b
    | total < 0 = Microseconds maxBound
    | otherwise = Microseconds total
   where
    total = a + b

instance Monoid Duration where
  mempty = Microseconds 0

microseconds :: Int64 -> Duration
microseconds = Microseconds . max 0

milliseconds :: Int64 -> Duration
milliseconds = times 1_000

seconds :: Int64 -> Duration
seconds = times 1_000_000

times :: Int64 -> Int64 -> Duration
times factor n
  | n <= 0                    = mempty
  | n > maxBound `div` factor = Microseconds maxBound
  | otherwise                 = Microseconds (n * factor)

toMicroseconds :: Duration -> Int64
toMicroseconds (Microseconds n) = n

{- | Meet the rest of the ecosystem, which measures time with @time@.
Negative input clamps to zero and anything past 'Int64' microseconds clamps
to the maximum, so this keeps the guarantee the smart constructors give.
Sub-microsecond precision is truncated.
-}
fromNominalDiffTime :: NominalDiffTime -> Duration
fromNominalDiffTime d
  | micros <= 0 = mempty
  | micros >= fromIntegral (maxBound @Int64) = Microseconds maxBound
  | otherwise = Microseconds $ truncate micros
 where
  micros :: Pico
  micros = nominalDiffTimeToSeconds d * 1_000_000

toNominalDiffTime :: Duration -> NominalDiffTime
toNominalDiffTime (Microseconds n) = secondsToNominalDiffTime $ fromIntegral n / 1_000_000

scale :: Double -> Duration -> Duration
scale factor (Microseconds n)
  | factor <= 0 = mempty
  | scaled >= fromIntegral (maxBound @Int64) = Microseconds maxBound
  | otherwise = Microseconds $ round scaled
 where
  scaled = factor * fromIntegral n

delay :: (Concurrent :> es) => Duration -> Eff es ()
delay (Microseconds n) = threadDelay . fromIntegral $ min n (fromIntegral (maxBound @Int))
