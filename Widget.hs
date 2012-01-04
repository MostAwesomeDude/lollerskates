module Widget where

import Import

import Data.Lens.Common

import Lol.Stats
import Lol.Stats.Types

coreStatsWidget :: CoreStats -> Widget
coreStatsWidget cs = [whamlet|
<ul>
    <li>Health: #{getL csHealth cs}
    <li>Mana: #{getL csMana cs}
    <li>Health Regen: #{getL csHealthRegen cs}
    <li>Mana Regen: #{getL csManaRegen cs}
    <li>AD: #{getL csAttackDamage cs}
    <li>Armor: #{getL csArmor cs}
    <li>MR: #{getL csMagicResist cs}
    <li>AS: #{getL csAttackSpeed cs}
    <li>Movement Speed: #{getL csMovementSpeed cs}
|]

extendedStatsWidget :: ExtendedStats -> Widget
extendedStatsWidget es = [whamlet|
<ul>
    <li>AP: #{getL esAbilityPower es}
    <li>Lifesteal: #{getL esLifeSteal es}
    <li>Vamp: #{getL esSpellVamp es}
    <li>Crit Chance: #{getL esCriticalChance es}
|]

champStatsWidget :: ChampStats -> Widget
champStatsWidget cs = [whamlet|
<h2>Champion Stats
^{coreStatsWidget $ getL cCoreStats cs}
^{extendedStatsWidget $ getL cExtendedStats cs}
|]

itemStatsWidget :: ItemStats -> Widget
itemStatsWidget is = [whamlet|
<h2>Item Stats
^{coreStatsWidget $ getL iCoreStats is}
^{extendedStatsWidget $ getL iExtendedStats is}
|]
