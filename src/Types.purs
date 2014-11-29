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

module Types where

import Data.Maybe
import Data.Map

import Monsters

data MessageType = Normal | Me | Status | System

type Time = String

data Message = Message
    { time :: Time
    , nick :: String
    , text :: String
    , msgType :: MessageType
    }

instance showMessage :: Show Message where
    show (Message msg) = (show msg.time) ++ " < " ++ msg.nick ++ " > " ++ msg.text

data User = User
    { nick :: String
    , hp :: Number
    , monster :: String
    }

type UserArrow = User -> User

userUser :: String -> User
userUser s = User { nick : s, hp : 80, monster : "human" }

monsterUser :: String -> User
monsterUser s = User { nick : s, hp : getMonsterHp s, monster : s }

userChangeHp :: (Number -> Number) -> UserArrow
userChangeHp f (User user) = User $ user { hp = f user.hp }

data Chat = Chat
    { users :: Map String User
    , messages :: [Message]
    , me :: Maybe String
    , time :: Number
    }

-- type ChatArrow a = Chat -> Tuple Chat a
type ChatArrow = Chat -> Chat

changeTime :: (Number -> Number) -> ChatArrow
changeTime f (Chat chat) = Chat $ chat { time = f chat.time }

changeUsers :: (Map String User -> Map String User) -> ChatArrow
changeUsers f (Chat chat) = Chat $ chat { users = f chat.users}

changeMe :: UserArrow -> ChatArrow
changeMe f (Chat chat) =
    case chat.me of
        Just me -> changeUser f me $ Chat chat
        Nothing -> Chat chat

changeUser :: UserArrow -> String -> ChatArrow
changeUser f name = id
