{-# LANGUAGE TemplateHaskell #-}

module Lol.Stats.Types where

import Prelude

import Data.Lens.Template

type Price = Int

data CoreStats = CoreStats { _csHealth :: Float
                           , _csMana :: Float
                           , _csAttackDamage :: Float
                           , _csAttackSpeed :: Float
                           , _csRange :: Float
                           , _csHealthRegen :: Float
                           , _csManaRegen :: Float
                           , _csArmor :: Float
                           , _csMagicResist :: Float
                           , _csMovementSpeed :: Float }
    deriving (Show)

$( makeLens ''CoreStats )

data ExtendedStats = ExtendedStats { _esAbilityPower :: Float
                                   , _esLifeSteal :: Float
                                   , _esSpellVamp :: Float
                                   , _esCriticalChance :: Float
                                   , _esBonusAttackSpeed :: Float
                                   , _esBonusMovementSpeed :: Float }
    deriving (Show)

$( makeLens ''ExtendedStats )
