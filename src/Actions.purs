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

module Actions where

import Types
import Chat
import Speaking

fightUser :: User -> Number -> ChatArrow
fightUser (User mob) n =
        changeMe (userChangeHp ((+) (-n)))
    >>> makeMessage Me mob.nick "hits you"

sendMessage :: User -> String -> ChatArrow
sendMessage (User user) = makeMessage Normal user.nick

meMessage :: User -> String -> ChatArrow
meMessage (User user) = makeMessage Me user.nick

statusMessage :: User -> String -> ChatArrow
statusMessage (User user) = makeMessage Status user.nick

speakingAct :: User -> String -> ChatArrow
speakingAct user = sendMessage user <<< randomAuthorQuote
