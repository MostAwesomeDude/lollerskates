module Lol.Helpers where

import Prelude

import System.Random

-- | Help create instances of Random for types which belong to Bounded and
--   Enum.
--   It was pointed out that it's not reasonable to always have (Bounded a,
--   Enum a) => Random a, so instead this helper is provided for types which
--   wish to be members of Random.
--   Just pop these helpers in for "random" and "randomR" in your instance.
boundedEnumRandomR :: (Bounded a, Enum a, RandomGen g) => (a, a) -> g -> (a, g)
boundedEnumRandomR (first, second) gen =
    let ifirst = fromEnum first
        isecond = fromEnum second
        repack (x, y) = (toEnum x, y)
    in repack $ randomR (ifirst, isecond) gen
boundedEnumRandom :: (Bounded a, Enum a, RandomGen g) => g -> (a, g)
boundedEnumRandom = boundedEnumRandomR (minBound, maxBound)
