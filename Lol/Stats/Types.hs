module Lol.Stats.Types where

import Prelude

type Price = Int

data CoreStats = CoreStats { csHealth :: Float
                           , csMana :: Float
                           , csAttackDamage :: Float
                           , csAttackSpeed :: Float
                           , csRange :: Float
                           , csHealthRegen :: Float
                           , csManaRegen :: Float
                           , csArmor :: Float
                           , csMagicResist :: Float
                           , csMovementSpeed :: Float }
    deriving (Show)

data ExtendedStats = ExtendedStats { esAbilityPower :: Float
                                   , esLifeSteal :: Float
                                   , esSpellVamp :: Float
                                   , esCriticalChance :: Float
                                   , esBonusMovementSpeed :: Float }
    deriving (Show)
