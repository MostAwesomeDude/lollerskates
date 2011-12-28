module Lol.Spells where

import Prelude

import System.Random

import Lol.Helpers

data Spell = Clairvoyance
           | Clarity
           | Cleanse
           | Exhaust
           | Flash
           | Ghost
           | Heal
           | Ignite
           | Promote
           | Revive
           | Smite
           | Surge
           | Teleport
    deriving (Bounded, Enum, Eq, Ord, Show)

instance Random Spell where
    randomR = boundedEnumRandomR
    random = boundedEnumRandom
