{-# LANGUAGE TemplateHaskell #-}
module Characters where

import Control.Lens

data Boss = Boss { _bossHP :: !Int
                 , _bossDmg :: !Int
                 , _bossPoisoned :: !Int } deriving (Show, Eq)

$(makeLenses ''Boss)

data Player = Player { _playerHP :: !Int
                     , _playerMana :: !Int
                     , _playerShield :: !Int
                     , _playerRecharge :: !Int
                     , _playerSpentMana :: !Int } deriving (Show, Eq)

$(makeLenses ''Player)


data Spell = Missile | Drain | Shield | Recharge | Poison deriving (Show, Eq, Enum)
type Mana = Int
data Move = Move !Spell !Mana deriving (Show, Eq)

mana (Move _ m) = m
