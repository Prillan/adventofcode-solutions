import Characters

import Control.Monad
import Control.Lens ((.~), (+~), (-~))
import Data.List (minimum)
import Data.Maybe (mapMaybe)
import Debug.Trace


checkBossHP p b cont
  | _bossHP b <= 0 = pure (_playerSpentMana p)
  | otherwise = cont

duel :: Player -> Boss -> Maybe Int -> (Player -> Player) -> [Int]
duel player boss limit hard = do
  guard (_playerHP player > 0)
  guard (not $ brokenLimit limit player)
  let (p, b) = (applyPlayerEffects . hard $ player, applyBossEffects boss)
  checkBossHP p b $ do

    (p', b') <- playerTurn p b limit
    guard (not $ brokenLimit limit p')
    guard (_playerHP p' > 0)
    checkBossHP p' b' $ do

      let (p'', b'') = (applyPlayerEffects p', applyBossEffects b')
      checkBossHP p'' b'' $ do

        (p''', b''') <- bossTurn p'' b''
        guard (_playerHP p''' > 0)
        duel p''' b''' limit hard

brokenLimit Nothing  _ = False
brokenLimit (Just l) p = _playerSpentMana p >= l

playerTurn p b l = do
    move <- validMoves p b
    pure $ playerMove move p b

bossTurn p b = pure (attack p, b)
  where attack p = (playerHP -~ dmg) p
        dmg = max 0 $ if _playerShield p >= 1 then _bossDmg b - 7 else _bossDmg b

applyPlayerEffects p = shield . recharge $ p
  where shield   = if _playerShield p >= 1 then (playerShield -~ 1) else id
        recharge = if _playerRecharge p >= 1
                     then (playerRecharge -~ 1) . (playerMana +~ 101)
                     else id
applyBossEffects b = poison $ b
  where poison = if _bossPoisoned b >= 1 then (bossHP -~ 3) . (bossPoisoned -~ 1) else id


playerMove (Move spell mana) player boss =
  ((pchange . (playerSpentMana +~ mana) . (playerMana -~ mana)) player
  , bchange boss)
  where (pchange, bchange) = case spell of
                               Poison   -> (id                 , bossPoisoned .~ 6)
                               Drain    -> (playerHP +~ 2      , bossHP -~ 2)
                               Shield   -> (playerShield .~ 6  , id)
                               Missile  -> (id                 , bossHP -~ 4)
                               Recharge -> (playerRecharge .~ 5, id)

validMoves :: Player -> Boss -> [Move]
validMoves p b = map (\(_, _, t, m) -> Move t m)
               . filter (\(pp, bp, _, m) -> pp p == 0 && bp b == 0 && _playerMana p >= m)
               $ moves

moves = [(const 0, _bossPoisoned, Poison, 173)
        ,(const 0, const 0, Missile, 53)
        ,(const 0, const 0, Drain, 73)
        ,(_playerShield, const 0, Shield, 113)
        ,(_playerRecharge, const 0, Recharge, 229)]

input = Boss { _bossHP = 51, _bossDmg = 9, _bossPoisoned = 0}

compute hard = go first
  where d = \l -> duel (Player 50 500 0 0 0) input l hard
        first = head $ d Nothing
        go l = case d (Just (l - 1)) of
                 []    -> l
                 (x:_) -> go x

part1 = compute id
part2 = compute (playerHP -~ 1)

main = do
  print part1
  print part2
