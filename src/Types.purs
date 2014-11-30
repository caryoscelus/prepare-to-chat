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
import Math (min)

import Debug

data MessageType = Normal | Me | Status | System | Unknown

type Time = Number

data Message = Message
    { time :: Time
    , nick :: String
    , text :: String
    , t :: MessageType
    }

instance showMessage :: Show Message where
    show (Message msg) =
        case msg.t of
            Normal  -> (show msg.time) ++ " < " ++ msg.nick ++ " > " ++ msg.text
            Me      -> (show msg.time) ++ " " ++ msg.nick ++ " " ++ msg.text
            _       -> (show msg.time) ++ "??" ++ msg.text

type UserT =
    { nick :: String
    , hp :: Number
    , maxHp :: Number
    , monster :: String
    , prepared :: Number
    }
data User = User UserT

type UserArrow = User -> User

user :: UserT
user =
    { nick : ""
    , hp : 0
    , maxHp : 0
    , monster : ""
    , prepared : 0
    }

userUser :: String -> User
userUser s = User $ user
    { nick = s
    , hp = 80
    , maxHp = 80
    }

userChangeHp :: (Number -> Number) -> UserArrow
userChangeHp f (User user) = User $ user { hp = min (f user.hp) user.maxHp }

userChangePrepared :: (Number -> Number) -> UserArrow
userChangePrepared f (User user) = User $ user { prepared = f user.prepared }

data Chat = Chat
    { users :: Map String User
    , messages :: [Message]
    , me :: String
    , time :: Time
    , inputLog :: [String]
    , inputNow :: String
    , inputLogA :: [String]
    }

-- type ChatArrow a = Chat -> Tuple Chat a
type ChatArrow = Chat -> Chat

changeTime :: (Number -> Number) -> ChatArrow
changeTime f (Chat chat) = Chat $ chat { time = f chat.time }

changeUsers :: (Map String User -> Map String User) -> ChatArrow
changeUsers f (Chat chat) = Chat $ chat { users = f chat.users}

changeMe :: UserArrow -> ChatArrow
changeMe f (Chat chat) = changeUser f chat.me $ Chat chat

changeUser :: UserArrow -> String -> ChatArrow
changeUser f name = changeUsers $ alter (maybe Nothing $ Just <<< f) name

changeLog :: ([String] -> [String]) -> ChatArrow
changeLog f (Chat chat) = Chat $ chat { inputLog = f chat.inputLog }
