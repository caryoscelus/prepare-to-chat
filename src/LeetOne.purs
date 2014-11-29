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

import Types
import Actions
import Eps

leetOne :: User
leetOne = User { nick : "1337 1", hp : 100500, monster : "1337" }

leetAct :: User -> ChatArrow
leetAct leet (Chat chat) = action $ Chat chat
  where
    action = case chat.time of
        t | ieq t 0 -> sendMessage leet $ "hello, " ++ chat.me ++ "!"
        t | ieq t 1 -> sendMessage leet "i am the l33t one!"
        otherwise   -> id
