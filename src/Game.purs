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

module Game where

import Data.Foldable
import Data.Array
import Data.Maybe
import qualified Data.Map as M
import Data.Tuple

import Types
import Util
import Chat
import Monsters
import Debug
import Eps
import Actions
import Random

processNPCs :: Number -> ChatArrow
processNPCs t = applyN t stepNPCs

stepNPCs :: ChatArrow
stepNPCs =
        killDead
    >>> changeTime ((+) 1)
    >>> monstersAct
    >>> killDead
--     >>> checkLooser
    >>> spawnMonsters
--     >>> checkWinner

killDead :: ChatArrow
killDead (Chat chat) = showDead (M.values >>> filter userDead $ chat.users) >>> removeDead $ Chat chat

showDead :: [User] -> ChatArrow
showDead = flip $ foldl (flip $ \user -> statusMessage user "dies and exits chat")

removeDead :: ChatArrow
removeDead = changeUsers $ M.toList >>> filter (not <<< userDead <<< snd) >>> M.fromList

userDead :: User -> Boolean
userDead (User user) = debug (user.nick ++ " " ++ show user.hp) $ user.hp <= 0

-- checkLooser :: ChatArrow
-- checkLooser = changeMe $ \mu ->
--     mu >>= \user -> if userDead user then Nothing else Just user

spawnMonsters :: ChatArrow
spawnMonsters (Chat chat) = addUsers (spawn $ spawnType chat.time) $ Chat chat

spawnType :: Number -> Maybe String
spawnType n | n < 4 = Nothing
spawnType n | n < 40 = if (n % 1 < eps) && (n % 5 < eps) then Just "rat" else Nothing
spawnType _ = Nothing

spawn :: Maybe String -> [User]
spawn Nothing = []
spawn (Just monsterType) = [monsterUser monsterType]

monstersAct :: ChatArrow
monstersAct (Chat chat) = foldl (flip monsterAct) (Chat chat) (M.values chat.users)

monsterAct :: User -> ChatArrow
monsterAct (User user) = getMonsterAct user.monster $ User user

-- checkWinner :: ChatArrow
-- checkWinner = id

processUserTurn :: String -> ChatArrow
processUserTurn s | startsWith "prepare" s = userPrepares
processUserTurn s | startsWith "hit" s = userHits s
processUserTurn s | startsWith "heal" s = userHeals
processUserTurn _ = id

userPrepares :: ChatArrow
userPrepares = changeMe $ userChangePrepared ((+)1)

userHits :: String -> ChatArrow
userHits s = readMe $ \(Just (User me)) ->
        changeUser (userChangeHp ((+)(-((1+me.prepared) * randomRange 2)))) name
    >>> changeMe (userChangePrepared (const 0))
  where
    name = consumeSpace >>> consumeUnspace >>> consumeSpace $ s

userHeals :: ChatArrow
userHeals = readMe $ \(Just (User me)) ->
        changeMe (userChangeHp ((+)(1+randomRange 1+me.prepared*3)))
    >>> changeMe (userChangePrepared (const 0))

monsterUser :: String -> User
monsterUser s = let mhp = getMonsterHp s in User $ user
    { nick = s
    , maxHp = mhp
    , hp = mhp
    , monster = s
    }
