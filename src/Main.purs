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
import Data.DOM.Simple.Element (setInnerHTML, value, setValue)
import Data.DOM.Simple.Window (globalWindow, document, location, getLocation)
import Data.DOM.Simple.Types
import Data.DOM.Simple.Events (UIEvent, UIEventType(..), addUIEventListener, KeyboardEvent, KeyboardEventType(..), addKeyboardEventListener, key)

import Chat
import Util
import Types
import Globals
import Game
import LeetOne

main = do
    addUIEventListener LoadEvent onLoad globalWindow
    addKeyboardEventListener KeypressEvent onPress globalWindow

onPress :: DOMEvent -> Eff (dom :: DOM, trace :: Trace, chate :: ChatE) Unit
onPress ev = do
    k <- key ev
    case k of
         "Enter"    -> doSendMessage
         "Up"       -> logRoll loopLogUp
         "Down"     -> logRoll loopLogDown
         otherwise  -> return unit
    return unit

logRoll :: forall t. (String -> ChatArrow) -> Eff (dom :: DOM, chate :: ChatE | t) Unit
logRoll f = do
    Just el <- queryElement "#chat_input_line"
    msg <- value $ el
    withChat $ f msg
    Chat ch <- getGlobalChat
    setValue ch.inputNow el

doSendMessage :: Eff (dom :: DOM, trace :: Trace, chate :: ChatE) Unit
doSendMessage = do
    Just el <- queryElement "#chat_input_line"
    msg <- value $ el
    let parsed = messageParse msg
    case parsed of
        (Tuple Normal "")   -> trace "empty message"
        (Tuple Normal txt)  -> do
            withChat $ userMessage Normal msg
            withChat $ processNPCs 1
        (Tuple Me txt)      -> do
            withChat $ userMessage Me msg
            withChat $ processUserTurn txt
            withChat $ processNPCs 1
        (Tuple Unknown "")  -> trace "unknown command"
        _                   -> trace "completely unknown command"
    ch <- getGlobalChat
    chatReload ch
    processLooseWin
    return unit

processLooseWin :: forall t. Eff (dom :: DOM, chate :: ChatE | t) Unit
processLooseWin = do
    Chat ch <- getGlobalChat
    if ch.lost
        then userLost
        else
            if ch.won
                then userWon
                else return unit

userLost :: forall t. Eff (dom :: DOM, chate :: ChatE | t) Unit
userLost = do
    alert "YOU LOST\nreload page to start over.."
    return unit

userWon :: forall t. Eff (dom :: DOM, chate :: ChatE | t) Unit
userWon = return unit

onLoad :: DOMEvent -> Eff (dom :: DOM, trace :: Trace, chate :: ChatE) Unit
onLoad _ = do
    chat <- setupChat
    setGlobalChat chat
    withChat $ monstersAct
    chat' <- getGlobalChat
    chatReload chat'
    return unit

setupChat :: forall t. Eff (dom :: DOM, trace :: Trace | t) Chat
setupChat = do
    name <- prompt "Your name?"
    return $ addUser leetOne $ useredChat name
