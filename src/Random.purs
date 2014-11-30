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

-- | THIS IS ABSURDLY UNSAFE IMPERATIVE RANDOM, DO NOT USE!
module Random where

-- importing Math overrides JS Math, so we import floor through Util
import Util (floor)

foreign import random1
    """
    function random1 (_) {
        return Math.random();
    }
    """ :: Unit -> Number

random :: Unit -> Number
random = random1

foreign import randomRange
    """
    function randomRange (n) {
        return Math.floor(Math.random()*(n+1));
    }
    """ :: Number -> Number
