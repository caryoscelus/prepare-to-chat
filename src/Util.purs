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

-- | TODO: replace with libs
module Util where

import Control.Monad.Eff

import Data.Maybe
import qualified Data.String as S
import Data.Char
import Data.Foldable

import Data.DOM.Simple.Document ()
import Data.DOM.Simple.Types
import Data.DOM.Simple.Window (globalWindow, document)
import Data.DOM.Simple.Element (querySelector)

foreign import promptI
    """
    function promptI(a) {
        return function(){
            return window.prompt(a)
        }
    }
    """ :: forall t. String -> Eff t String

prompt :: forall t. String -> Eff t String
prompt = promptI

queryElement :: forall t. String -> Eff (dom :: DOM | t) (Maybe HTMLElement)
queryElement s = document globalWindow >>= querySelector s

foreign import scrollToEnd
    """
    function scrollToEnd (e) {
        return function () {
            e.scrollTop = e.scrollHeight;
        }
    }
    """ :: forall t. HTMLElement -> Eff (dom :: DOM | t) Unit

-- strings
startsWith :: String -> String -> Boolean
startsWith pat s = S.take (S.length pat) s == pat

isSpace :: Char -> Boolean
isSpace c | charString c == " " = true
isSpace c | charString c == "\t" = true
isSpace c | charString c == "\n" = true
isSpace _ = false

consumeSpace :: String -> String
consumeSpace = S.dropWhile isSpace

consumeUnspace :: String -> String
consumeUnspace = S.dropWhile (not <<< isSpace)

addLead :: String -> Number -> String -> String
addLead ch n s = S.joinWith "" (repeatN (n - S.length s) ch) ++ s


-- arrays
repeatN :: forall a. Number -> a -> [a]
repeatN n a | n < 1 = []
repeatN n a = a : repeatN (n-1) a

applyN :: forall a. Number -> (a -> a) -> a -> a
applyN n f a = foldl (flip ($)) a $ repeatN n f
