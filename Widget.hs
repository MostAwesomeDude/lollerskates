module Widget where

import Import

import Lol.Stats
import Lol.Stats.Types

coreStatsWidget :: CoreStats -> Widget
coreStatsWidget cs = [whamlet|
<ul>
    <li>Health: #{csHealth cs}
    <li>Mana: #{csMana cs}
    <li>Health Regen: #{csHealthRegen cs}
    <li>Mana Regen: #{csManaRegen cs}
    <li>AD: #{csAttackDamage cs}
    <li>Armor: #{csArmor cs}
    <li>MR: #{csMagicResist cs}
    <li>AS: #{csAttackSpeed cs}
    <li>Movement Speed: #{csMovementSpeed cs}
|]

extendedStatsWidget :: ExtendedStats -> Widget
extendedStatsWidget es = [whamlet|
<ul>
    <li>AP: #{esAbilityPower es}
    <li>Lifesteal: #{esLifeSteal es}
    <li>Vamp: #{esSpellVamp es}
    <li>Crit Chance: #{esCriticalChance es}
|]

champStatsWidget :: ChampStats -> Widget
champStatsWidget cs = [whamlet|
<h2>Champion Stats
^{coreStatsWidget $ cCoreStats cs}
^{extendedStatsWidget $ cExtendedStats cs}
|]

itemStatsWidget :: ItemStats -> Widget
itemStatsWidget is = [whamlet|
<h2>Item Stats
^{coreStatsWidget $ iCoreStats is}
^{extendedStatsWidget $ iExtendedStats is}
|]
