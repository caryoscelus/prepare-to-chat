--
--  Copyright (C) 2014 caryoscelus
--  
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--  
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--  
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

module Monsters where

import qualified Data.String as S
import qualified Math as M

import Actions
import Types
import LeetOne
import Random
import Util
import Speaking

-- !! UNSAFE RANDOM
getMonsterHp :: String -> Number
getMonsterHp "rat" = 5 + randomRange 3
getMonsterHp "weak rat" = 3 + randomRange 2
getMonsterHp s | startsWith "speaking " s = 17 + randomRange 8
getMonsterHp _ = 1

getMonsterAct :: String -> User -> ChatArrow
getMonsterAct "1337" leet = leetAct leet
getMonsterAct "rat" rat = fightUser rat (randomRange 2)
getMonsterAct s mob | startsWith "speaking " s = speakingAct mob (S.drop 9 s)
getMonsterAct _ _ = id

speakingAct :: User -> String -> ChatArrow
speakingAct mob author =
        sendMessage mob quote
    >>> fightUser mob ((baseHit + randomRange baseHit)/10)
  where
    quote = randomAuthorQuote author
    baseHit = M.min (S.length quote) 20
