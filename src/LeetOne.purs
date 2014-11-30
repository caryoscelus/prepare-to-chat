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

module LeetOne where

import Data.Maybe
import Data.Array

import Types
import Actions
import Eps
import Random

leetOne :: User
leetOne = User $ user
    { nick = "1337 1"
    , hp = 100500
    , maxHp = 100500
    , monster = "1337"
    }

healMessages =
    [ "drinks white potion"
    , "zaps a quaint wand"
    , "casts spell"
    , "kneels and offers prayer to some god"
    ]

hurtMessages =
    [ ", why d0 U hurt me?!"
    , ", WHy Do Y0u hUr1 m3!11"
    , ", Th4t hUrts!"
    , ", St0p d01ng THIS!!!"
    , ", U R MORON!"
    , ", U b100dy B4ST4RD!"
    , ", you will pay for this..."
    ]

killMessages =
    [ "come on, kill that "
    , "HIT THAT "
    , "eeh, can't you kill "
    , "please, kill "
    , "stop shambling around, kill "
    , "may i ask you to assassinate "
    ]

-- UNSAFE
randomMessage :: [String] -> String
randomMessage s = maybe "does something" id $ s !! randomRange (length s - 1)

leetAct :: User -> ChatArrow
leetAct leet (Chat chat) =
        hurtAction
    >>> timedAction
    >>> killMonsterAction "weak rat"
    >>> killMonsterAction "rat"
    >>> killMonsterAction "rat_"
      $ Chat chat

  where
    
    hurtAction = case leet of
        User u | u.hp /= u.maxHp ->
                changeUser (userChangeHp $ const u.maxHp) u.nick
            >>> sendMessage leet (chat.me ++ randomMessage hurtMessages)
            >>> meMessage leet (randomMessage healMessages)
            >>> meMessage leet "is completely healed"
        _ -> id
    
    timedAction = case chat.time of
        t | ieq t 0     -> sendMessage leet $ "hello, " ++ chat.me ++ ", welcome to our humble chat!"
        t | ieq t 1     -> sendMessage leet "i am the l33t one!"
        t | ieq t 2     -> meMessage leet "will guide you to the victory!"
        t | ieq t 3     -> sendMessage leet "so, first thing you need to know:"
                   >>> sendMessage leet "you need to fight to survive"
        t | ieq t 4     -> sendMessage leet "if you're ready, i'll show you example"
        t | ieq t 5     -> sendMessage leet "see this weak rat? try to *hit* it so that it doesn't hit you!"
        t | ieq t 9     -> sendMessage leet "now that rat was a weak one. real rats are tougher"
        t | ieq t 10    -> sendMessage leet "to fight other monsters effectively, you must *prepare*"
        t | ieq t 11     -> sendMessage leet "and if you are in deep trouble, try to *heal*"
        t | ieq t 12    -> sendMessage leet "now fight and don't get killed!"
        t | ieq t 40    -> meMessage leet "feels someone powerful arriving!"
        otherwise       -> id
    
    killMonsterAction name = readUser (\user ->
        if random unit > 0.6
            then
                case user of
                    Just _  -> sendMessage leet $ randomMessage killMessages ++ name
                    Nothing -> id
            else
                id
        ) name
