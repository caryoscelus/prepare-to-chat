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
import Control.Apply ((*>))

import Data.Foldable
import Data.Traversable
import Data.Array
import Data.Maybe
import qualified Data.Map as M
-- import Data.Tuple

import Data.DOM.Simple.Types
import Data.DOM.Simple.Element (querySelector, setInnerHTML, focus)
import Data.DOM.Simple.Window (globalWindow, document, location, getLocation)

import Text.Smolder.HTML (div, h1, h2, p, form, input, br, textarea)
import Text.Smolder.HTML.Attributes (className, type', value)
import Text.Smolder.Markup (text, attribute, (!), Markup(), MarkupM(..))
import Text.Smolder.Renderer.String (render)

import Util
import Types

emptyChat :: Chat
emptyChat = Chat { users : M.fromList [], messages : [], me : Nothing, time : 0 }

addUser :: User -> ChatArrow
addUser (User user) (Chat chat) =
    if M.member user.nick chat.users
        then addUser (User $ user { nick = user.nick++"_" } ) (Chat chat)
        else Chat $ chat { users = M.insert user.nick (User user) chat.users }

addUsers :: [User] -> ChatArrow
addUsers users chat = foldl (flip addUser) chat users

setMe :: Maybe String -> ChatArrow
setMe me (Chat chat) = Chat $ chat { me = me }

putMessage :: Message -> ChatArrow
putMessage msg (Chat chat) = Chat $ chat { messages = chat.messages ++ [msg] }

makeMessage :: String -> String -> ChatArrow
makeMessage nick text = putMessage $ Message
    { time : "??"
    , nick : nick
    , text : text
    , msgType : Normal
    }

chatReload :: forall t. Chat -> Eff (dom :: DOM, trace :: Trace | t) Unit
chatReload chat = do
    Just chatDiv <- queryElement "#chat"
    setInnerHTML (fullChatRender chat) chatDiv
    chatFocus
    return unit

chatFocus :: forall t. Eff (dom :: DOM, trace :: Trace | t) Unit
chatFocus = do
    Just chatInput <- queryElement "#chat_input_line"
    focus chatInput
    return unit

-- | ask for name and start chat
setupChat :: forall t. Eff (dom :: DOM, trace :: Trace | t) Chat
setupChat = do
    name <- prompt "Your name?"
    let user = userUser name
    let chat = setMe (Just name) <<< addUser user $ emptyChat
    chatReload chat
    return chat

attrId = attribute "id"
span' = Parent "span"

-- for some reason, this seems to be NOT equal to *> from Control.Apply
(>>) :: forall m a b. (Monad m) => m a -> m b -> m b
(>>) a b = do
    _ <- a
    b

userGetNick :: User -> String
userGetNick (User u) = u.nick

fullChatRender :: Chat -> String
fullChatRender (Chat chat) = render $ do
    div ! attrId "chat_main" $ do
        div ! attrId "chat_messages" $
            div ! attrId "chat_messages_wrap" $
                foldl (>>) (return unit) $ map ((p ! className "message") <<< text <<< show) chat.messages
        div ! attrId "chat_users" $ foldl (>>) (return unit) $ map ((p ! className "user") <<< text) $ M.keys chat.users
    div ! attrId "chat_input" $ do
        span' ! attrId "chat_input_nick" $ text $ "< "++ maybe "??" id chat.me ++" >"
        input ! attrId "chat_input_line" ! type' "text"
