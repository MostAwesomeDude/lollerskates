module Lol.Champs where

import Prelude

import System.Random

import Lol.Helpers

data Champ = Ahri
           | Akali
           | Alistar
           | Amumu
           | Anivia
           | Annie
           | Ashe
           | Blitzcrank
           | Brand
           | Caitlyn
           | Cassiopeia
           | Chogath
           | Corki
           | DrMundo
           | Evelynn
           | Ezreal
           | Fiddlesticks
           | Fizz
           | Galio
           | Gangplank
           | Garen
           | Gragas
           | Graves
           | Heimerdinger
           | Irelia
           | Janna
           | JarvanIV
           | Jax
           | Karma
           | Karthus
           | Kassadin
           | Katarina
           | Kayle
           | Kennan
           | Kogmaw
           | LeBlanc
           | LeeSin
           | Leona
           | Lux
           | Malphite
           | Malzahar
           | Maokai
           | MasterYi
           | MissFortune
           | Mordekaiser
           | Morgana
           | Nasus
           | Nidalee
           | Nocturne
           | Nunu
           | Olaf
           | Orianna
           | Pantheon
           | Poppy
           | Rammus
           | Renekton
           | Riven
           | Rumble
           | Ryze
           | Shaco
           | Shen
           | Shyvana
           | Singed
           | Sion
           | Sivir
           | Skarner
           | Sona
           | Soraka
           | Swain
           | Talon
           | Taric
           | Teemo
           | Tristana
           | Trundle
           | Tryndamere
           | TwistedFate
           | Twitch
           | Udyr
           | Urgot
           | Vayne
           | Veigar
           | Vladimir
           | Volibear
           | Warwick
           | Wukong
           | Xerath
           | XinZhao
           | Yorick
           | Zilean
    deriving (Bounded, Enum, Eq, Ord, Show)

instance Random Champ where
    randomR = boundedEnumRandomR
    random = boundedEnumRandom
