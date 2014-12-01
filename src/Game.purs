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
import qualified Data.String as S
import Data.Tuple

import Types
import Util
import Chat
import Monsters
import Debug
import Eps
import Actions
import Random
import Speaking

processNPCs :: Number -> ChatArrow
processNPCs t = applyN t stepNPCs

stepNPCs :: ChatArrow
stepNPCs =
        killDead
    >>> changeTime ((+) 1)
    >>> monstersAct
    >>> killDead
    >>> checkLooser
    >>> spawnMonsters
    >>> checkWinner

killDead :: ChatArrow
killDead (Chat chat) = showDead dUsers >>> gainExpDead dUsers >>> removeDead $ Chat chat
  where
    dUsers = M.values >>> filter userDead $ chat.users

gainExpDead :: [User] -> ChatArrow
gainExpDead l = changeMe $ gainExp (length l)

showDead :: [User] -> ChatArrow
showDead = flip $ foldl (flip $ \user -> statusMessage user "dies and exits chat")

removeDead :: ChatArrow
removeDead = changeUsers $ M.toList >>> filter (not <<< userDead <<< snd) >>> M.fromList

userDead :: User -> Boolean
userDead (User user) = debug (user.nick ++ " " ++ show user.hp) $ user.hp <= 0

checkLooser :: ChatArrow
checkLooser = readMe $ \user ->
    case user of
        Just u  -> id
        Nothing -> changeLost (const true)

spawnMonsters :: ChatArrow
spawnMonsters (Chat chat) = addUsers (spawn $ spawnType chat.time) $ Chat chat

spawnType :: Number -> Maybe (Tuple String Number)
spawnType n | n < 12 = if ieq n 5 then Just (Tuple "weak rat" 0) else Nothing
spawnType n | n < 27 = if (n % 1 < eps) && (n % 5 < eps) then Just (Tuple "rat" (randomRange 2)) else Nothing
spawnType n | n < 28 = if ieq n 27 then Just (Tuple ("speaking "++randomAuthor unit) 1) else Nothing
spawnType n | n < 48 = if ieq n 47 then Just (Tuple ("speaking "++randomAuthor unit) 3) else Nothing
spawnType n | n < 68 = if ieq n 67 then Just (Tuple ("speaking "++randomAuthor unit) 5) else Nothing
spawnType _ = Nothing

spawn :: Maybe (Tuple String Number) -> [User]
spawn Nothing = []
spawn (Just (Tuple monsterType lvl)) = [monsterUser monsterType lvl]

monstersAct :: ChatArrow
monstersAct (Chat chat) = foldl (flip monsterAct) (Chat chat) (M.values chat.users)

monsterAct :: User -> ChatArrow
monsterAct (User user) = getMonsterAct user.monster $ User user

checkWinner :: ChatArrow
checkWinner (Chat chat) =
    if chat.time > 67 && (length $ M.values chat.users) == 2 && not chat.lost
       then Chat $ chat { won = true }
       else Chat chat

processUserTurn :: String -> ChatArrow
processUserTurn s | startsWith "prepare" s = userPrepares
processUserTurn s | startsWith "hit" s = userHits s
processUserTurn s | startsWith "heal" s = userHeals
processUserTurn _ = id

userPrepares :: ChatArrow
userPrepares = changeMe $ userChangePrepared ((+)1)

userHits :: String -> ChatArrow
userHits s = readMe $ \(Just (User me)) ->
        changeUser (userChangeHp ((+)(-((1+randomRange (me.prepared*2))*me.level)))) name
    >>> changeMe (userChangePrepared (const 0))
  where
    name = consumeSpace >>> consumeUnspace >>> consumeSpace $ s

userHeals :: ChatArrow
userHeals = readMe $ \(Just (User me)) ->
        changeMe (userChangeHp ((+)((1+randomRange me.prepared+me.prepared+me.level*2)*3)))
    >>> changeMe (userChangePrepared (const 0))

stripSpeaking :: String -> String
stripSpeaking s =
    if startsWith "speaking " s
        then S.drop 9 s
        else s

monsterUser :: String -> Number -> User
monsterUser s lvl = let mhp = getMonsterHp s in User $ user
    { nick = stripSpeaking s
    , maxHp = mhp
    , hp = mhp
    , monster = s
    , level = lvl
    }
