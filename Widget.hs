module Widget where

import Import

import Lol.Stats

itemStatsWidget :: ItemStats -> Widget
itemStatsWidget s = [whamlet|
<h2>Stats
<ul>
    <li>Price: #{price s}
    <li>Health: #{health s}
    <li>Health Regen: #{healthRegen s}
    <li>Mana: #{mana s}
    <li>Mana Regen: #{manaRegen s}
    <li>AD: #{attackDamage s}
    <li>AP: #{abilityPower s}
    <li>Armor: #{armor s}
    <li>MR: #{magicResist s}
    <li>Lifesteal: #{lifeSteal s}
    <li>Vamp: #{spellVamp s}
    <li>AS: #{attackSpeed s}
    <li>Crit Chance: #{criticalChance s}
    <li>Movement Speed: #{movementSpeed s}
|]
