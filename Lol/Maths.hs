module Lol.Maths where

import Prelude

import Lol.Stats.Types

-- Throughout this module, the verb "to finalize" is specifically used to
-- refer to actions which result in the final, complete tally of a statistic.

-- | Turn a "raw" movement speed number into a final movement speed, by
--   applying a handful of equalizers which pull it towards [220, 415].
equalizeMovementSpeed :: Float -> Float
equalizeMovementSpeed ms | ms < 220  = ms * 0.5 + 110
                         | ms > 490  = ms * 0.5 + 230
                         | ms > 415  = ms * 0.8 + 83
                         | otherwise = ms

-- | Sum up movement speed bonuses for given core and extended stats.
--   The formula is to sum up base MS, multiply by percentage bonus MS, and
--   then apply an equalizer.
--   MS_f = equalize((MS_b + MS_flat) * sum(MS_bonus))
finalizeMovementSpeed :: CoreStats -> ExtendedStats -> CoreStats
finalizeMovementSpeed core extended =
    let base = csMovementSpeed core
        bonus = 1 + esBonusMovementSpeed extended
    in core { csMovementSpeed = equalizeMovementSpeed $ base * bonus }