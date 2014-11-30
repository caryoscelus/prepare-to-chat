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
import qualified Data.Map as M

import Data.DOM.Simple.Document ()
import Data.DOM.Simple.Element (setInnerHTML, value, setValue)
import Data.DOM.Simple.Window (globalWindow, document, location, getLocation)
import Data.DOM.Simple.Types
import Data.DOM.Simple.Events (UIEvent, UIEventType(..), addUIEventListener, KeyboardEvent, KeyboardEventType(..), addKeyboardEventListener, key, keyCode)

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
    k' <- keyCode ev
    let isEnter = k == "Enter" || k' == 13
    case k of
         _ | isEnter-> doSendMessage
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
        (Tuple Help _)      -> do
            return unit
            withChat $ makeMessage Normal "1337 1" "here's how to use chat:"
            withChat $ makeMessage Normal "1337 1" "enter message and press enter."
            withChat $ makeMessage Normal "1337 1" "To do something, use \"/me [action] [object]\" command.."
        (Tuple Unknown "")  -> trace "unknown command"
        _                   -> trace "completely unknown command"
    ch <- getGlobalChat
    chatReload ch
    userStatusReload ch
    processLooseWin
    return unit

userStatusReload :: forall t. Chat -> Eff (dom :: DOM | t) Unit
userStatusReload chat = do
    Just div <- queryElement "#player_status"
    setInnerHTML (renderStatus chat) div

renderStatus :: Chat -> String
renderStatus (Chat chat) =
    case M.lookup chat.me chat.users of
        Just user   -> "[you are alive ("++hpMsg user++"), "++prepMsg user++" ]"
        Nothing     -> "[you are dead]"
  where
    hpMsg (User user) = show user.hp ++ "/" ++ show user.maxHp
    prepMsg (User user) = case user.prepared of
        0 -> "you are not prepared"
        1 -> "you are slightly prepared"
        2 -> "you are quite prepared"
        3 -> "you are prepared"
        _ -> "you are PREPARED"

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
    alert "YOU HAVE LOST\nreload page to start over.."
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
    userStatusReload chat'
    return unit

setupChat :: forall t. Eff (dom :: DOM, trace :: Trace | t) Chat
setupChat = do
    name <- prompt "Your name?"
    return $ addUser leetOne $ useredChat name

iAmStuck :: forall t. Eff (dom :: DOM | t) Unit
iAmStuck = do
    Just div <- queryElement "#hint"
    setInnerHTML "<p>Type /help and hit Enter.</p>" div
    return unit
