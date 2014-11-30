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

module Chat where

import Debug.Trace (Trace(), trace)

import Control.Monad.Eff

import Data.Foldable
import Data.Traversable
import Data.Array
import Data.Maybe
import qualified Data.Map as M
import Data.Tuple

import Data.DOM.Simple.Types
import Data.DOM.Simple.Element (querySelector, setInnerHTML, focus, setAttribute)
import Data.DOM.Simple.Window (globalWindow, document, location, getLocation, innerHeight)

import Text.Smolder.HTML (div, h1, h2, p, form, input, br, textarea)
import Text.Smolder.HTML.Attributes (className, type', value)
import Text.Smolder.Markup (text, attribute, (!), Markup(), MarkupM(..))
import Text.Smolder.Renderer.String (render)

import Util
import Types
import Eps

emptyChat :: Chat
emptyChat = Chat { users : M.fromList [], messages : [], me : "??", time : 0, inputLog : [], inputLogA : [], inputNow : "", lost : false, won : false }

addUser :: User -> ChatArrow
addUser (User user) (Chat chat) =
    if M.member user.nick chat.users
        then addUser (User $ user { nick = user.nick++"_" } ) (Chat chat)
        else makeMessage Status user.nick "enters chat" $ Chat $ chat { users = M.insert user.nick (User user) chat.users }

addUsers :: [User] -> ChatArrow
addUsers users chat = foldl (flip addUser) chat users

setMe :: String -> ChatArrow
setMe me (Chat chat) = Chat $ chat { me = me }

putMessage :: Message -> ChatArrow
putMessage msg (Chat chat) = Chat $ chat { messages = chat.messages ++ [msg] }

makeMessage :: MessageType -> String -> String -> ChatArrow
makeMessage t nick text (Chat chat) = putMessage (Message
    { time : chat.time
    , nick : nick
    , text : text
    , t    : t
    }) $ Chat chat

messageParse :: String -> Tuple MessageType String
messageParse s =
    case s of
        _ | startsWith "/me " s -> Tuple Me (rest s)
        _ | startsWith "/" s    -> Tuple Unknown s
        _                       -> Tuple Normal s
  where
    rest = consumeUnspace >>> consumeSpace

userMessage :: MessageType -> String -> ChatArrow
userMessage t msg (Chat chat) =
        makeMessage t chat.me (messageParse >>> snd $ msg)
    >>> reLog
    >>> changeLog ((:) msg)
      $ Chat chat

reLog :: ChatArrow
reLog (Chat chat) = Chat $ chat
    { inputLog = reverse chat.inputLogA ++ chat.inputLog
    , inputLogA = []
    }

loopLogUp :: String -> ChatArrow
loopLogUp s (Chat chat) =
    if chat.inputLog /= []
        then Chat $ chat
            { inputLog = drop 1 chat.inputLog
            , inputNow = maybe "??" id (head chat.inputLog)
            , inputLogA = s : chat.inputLogA
            } 
        else Chat $ chat
            { inputNow = s
            }

loopLogDown :: String -> ChatArrow
loopLogDown s (Chat chat) =
    if chat.inputLogA /= []
        then Chat $ chat
            { inputLogA = drop 1 chat.inputLogA
            , inputNow = maybe "??" id (head chat.inputLogA)
            , inputLog = s : chat.inputLog
            } 
        else Chat chat

chatReload :: forall t. Chat -> Eff (dom :: DOM, trace :: Trace | t) Unit
chatReload chat = do
    Just chatDiv <- queryElement "#chat"
    setInnerHTML (fullChatRender chat) chatDiv
    chatFocus
    chatResize
    chatScroll
    return unit

chatResize :: forall t. Eff (dom :: DOM, trace :: Trace | t) Unit
chatResize = do
    Just div <- queryElement "#chat_messages_wrap"
    height <- innerHeight globalWindow
    trace $ show height
    setAttribute "style" ("height:" ++ show (height*0.7) ++ "px") div

chatScroll :: forall t. Eff (dom :: DOM | t) Unit
chatScroll = do
    Just chatMsgDiv <- queryElement "#chat_messages_wrap"
    scrollToEnd chatMsgDiv

chatFocus :: forall t. Eff (dom :: DOM, trace :: Trace | t) Unit
chatFocus = do
    Just chatInput <- queryElement "#chat_input_line"
    focus chatInput
    return unit

useredChat :: String -> Chat
useredChat name = setMe name <<< addUser (userUser name) $ emptyChat

attrId = attribute "id"
span' = Parent "span"

-- for some reason, this seems to be NOT equal to *> from Control.Apply
(>>) :: forall m a b. (Monad m) => m a -> m b -> m b
(>>) a b = do
    _ <- a
    b

userGetNick :: User -> String
userGetNick (User u) = u.nick

showTime :: Number -> String
showTime = show >>> addLead "0" 4

renderMessage :: Message -> Markup
renderMessage (Message msg) =
    case msg.t of
        Normal -> do
            span' ! className "msg_date" $ text $ showTime msg.time
            span' ! className "msg_angl" $ text " <"
            span' ! className "msg_nick" $ text msg.nick
            span' ! className "msg_angl" $ text "> "
            span' ! className "msg_text" $ text msg.text
        Me -> do
            span' ! className "msg_date" $ text $ showTime msg.time
            span' ! className "msg_me"   $ text $ " * " ++ msg.nick ++ " " ++ msg.text
        Status -> do
            span' ! className "msg_date" $ text $ showTime msg.time
            span' ! className "msg_stat" $ text $ " * " ++ msg.nick ++ " " ++ msg.text
        _ -> do
            span' ! className "msg_date" $ text $ showTime msg.time
            text " "
            span' ! className "msg_text" $ text $ msg.nick ++ " ?? " ++ msg.text

renderUser :: User -> Markup
renderUser (User user) = do
    span' ! className "user_nick" $ text user.nick
    case user.hp / user.maxHp of
        n | ieq n 1 -> return unit
        n | n > 0.8 -> wounded "lightly wounded"
        n | n > 0.5 -> wounded "moderately wounded"
        n | n > 0.3 -> wounded "severely wounded"
        n | n > 0   -> wounded "almost dead"
        _           -> wounded "walking corpse"
  where
    wounded t = span' ! className "user_stat" $ text $ " (" ++ t ++ ")"

fullChatRender :: Chat -> String
fullChatRender (Chat chat) = render $ do
    div ! attrId "chat_main" $ do
        div ! attrId "chat_messages" $ do
            h2 $ text "Messages"
            div ! attrId "chat_messages_wrap" $
                foldl (>>) (return unit) $ map ((p ! className "message") <<< renderMessage) chat.messages
        div ! attrId "chat_users" $ do
            h2 $ text "Users"
            div ! attrId "chat_users_wrap" $
                foldl (>>) (return unit) $ map ((p ! className "user") <<< renderUser) $ M.values chat.users
    div ! attrId "chat_input" $ do
        span' ! attrId "chat_input_nick" $ text $ "< "++ chat.me ++" >"
        input ! attrId "chat_input_line" ! type' "text"
