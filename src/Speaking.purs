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

module Speaking where

import Control.Monad.Eff

-- we have nice quote collection in _allQuotes
-- here are some helper functions to retrieve them

foreign import data Quote :: !

foreign import getRandomAuthor
    """
    function getRandomAuthor () {
        var _Mkeys = Object.keys(_allQuotes);
        var n = Math.floor(_Mkeys.length * Math.random());
        return _Mkeys[n];
    }
    """ :: forall t. Eff (quote :: Quote | t) String

foreign import getRandomAuthorQuote
    """
    function getRandomAuthorQuote (author) {
        return function () {
            var _Mquotes = _allQuotes[author];
            var n = Math.floor(_Mquotes.length * Math.random());
            return _Mquotes[n];
        }
    }
    """ :: forall t. String -> Eff (quote :: Quote | t) String

getCompletelyRandomQuote :: forall t. Eff (quote :: Quote | t) String
getCompletelyRandomQuote = do
    author <- getRandomAuthor
    getRandomAuthorQuote author
