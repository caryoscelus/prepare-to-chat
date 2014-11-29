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

module Main where

import Debug.Trace (Trace(), trace)

import Control.Monad.Eff

import Data.Maybe
import Data.Tuple
import Data.Array
import qualified Data.String as S

import Data.DOM.Simple.Document ()
import Data.DOM.Simple.Element (setInnerHTML, value)
import Data.DOM.Simple.Window (globalWindow, document, location, getLocation)
import Data.DOM.Simple.Types
import Data.DOM.Simple.Events (UIEvent, UIEventType(..), addUIEventListener, KeyboardEvent, KeyboardEventType(..), addKeyboardEventListener, key)

import Chat
import Util
import Types
import Globals
import Game

main = do
    addUIEventListener LoadEvent onLoad globalWindow
    addKeyboardEventListener KeypressEvent onPress globalWindow

onPress :: DOMEvent -> Eff (dom :: DOM, trace :: Trace, chate :: ChatE) Unit
onPress ev = do
    k <- key ev
    case k of
         "Enter" -> doSendMessage
         otherwise -> return unit
    return unit

doSendMessage :: Eff (dom :: DOM, trace :: Trace, chate :: ChatE) Unit
doSendMessage = do
    Just el <- queryElement "#chat_input_line"
    msg <- value $ el
    case msg of
        ""                          -> trace "empty message"
        _ | startsWith "/me " msg   -> do
            Chat ch <- getGlobalChat
            withChat $ makeMessage (maybe nullUser id ch.me) msg
            withChat $ processUserTurn $ S.drop 4 msg
            withChat $ processNPCs 1.0
        _ | startsWith "/" msg      -> trace "unknown command"
        otherwise                   -> do
            Chat ch <- getGlobalChat
            withChat $ makeMessage (maybe nullUser id ch.me) msg
            withChat $ processNPCs 1.0
    ch <- getGlobalChat
    chatReload ch
    return unit

onLoad :: DOMEvent -> Eff (dom :: DOM, trace :: Trace, chate :: ChatE) Unit
onLoad _ = do
    chat <- setupChat
    setGlobalChat chat
    return unit
