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

-- TODO: Replace with proper state monads..

module Globals where

import Control.Monad.Eff

import Types

foreign import data ChatE :: !

foreign import setGlobalChat
    """
    function setGlobalChat (ch) {
        return function () {
            _globalChat = ch;
        }
    }
    """ :: forall t. Chat -> Eff (chate :: ChatE | t) Unit

foreign import getGlobalChat
    """
    function getGlobalChat () {
        return _globalChat;
    }
    """ :: forall t. Eff (chate :: ChatE | t) Chat

withChat :: forall t. ChatArrow -> Eff (chate :: ChatE | t) Unit
withChat f = do
    ch <- getGlobalChat
    setGlobalChat $ f ch
