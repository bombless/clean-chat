implementation module ChatCore

import StdEnv

:: Role = System | User | Assistant

:: Message =
    { role    :: !Role
    , content :: !String
    }

:: ChatState =
    { model       :: !String
    , baseUrl     :: !String
    , temperature :: !Real
    , stream      :: !Bool
    , messages    :: ![Message]
    }

newChat :: !String !String !Real !Bool -> ChatState
newChat model baseUrl temperature stream =
    { model       = model
    , baseUrl     = baseUrl
    , temperature = temperature
    , stream      = stream
    , messages    = []
    }

appendMessage :: !Role !String !ChatState -> ChatState
appendMessage role content state =
    { state & messages = state.messages ++ [{role = role, content = content}] }

setSystemPrompt :: !String !ChatState -> ChatState
setSystemPrompt prompt state = case state.messages of
    [{role = System}:rest] = { state & messages = [{role = System, content = prompt}:rest] }
    msgs                   = { state & messages = [{role = System, content = prompt}:msgs] }

dropLastMessage :: !ChatState -> ChatState
dropLastMessage state = { state & messages = dropLast state.messages }
where
    dropLast [] = []
    dropLast [_] = []
    dropLast [x:xs] = [x:dropLast xs]

clearChat :: !ChatState -> ChatState
clearChat state = { state & messages = [{role = System, content = "You are a helpful assistant."}] }

roleName :: !Role -> String
roleName System    = "system"
roleName User      = "user"
roleName Assistant = "assistant"
