module Lol.Maths where

import Prelude

import Data.Lens.Common

import Lol.Stats.Types

-- Throughout this module, the verb "to finalize" is specifically used to
-- refer to actions which result in the final, complete tally of a statistic.

-- | Clamp attack speed.
equalizeAttackSpeed :: Float -> Float
equalizeAttackSpeed = min 2.5

-- | Sum up movement speed bonuses for given core and extended stats.
--   The formula is to sum up base AS, multiply by percentage bonus AS, and
--   then apply an equalizer.
--   AS_f = equalize((AS_b + AS_flat) * sum(AS_bonus))
finalizeAttackSpeed :: CoreStats -> ExtendedStats -> CoreStats
finalizeAttackSpeed core extended =
    let bonus = 1 + (esBonusAttackSpeed ^$ extended)
        equalizer = equalizeAttackSpeed . (bonus *)
    in csAttackSpeed ^%= equalizer $ core

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
    let bonus = 1 + (esBonusMovementSpeed ^$ extended)
        equalizer = equalizeMovementSpeed . (bonus *)
    in csMovementSpeed ^%= equalizer $ core

-- | Clamp critical chance.
finalizeCriticalChance :: Float -> Float
finalizeCriticalChance = min 1.0
