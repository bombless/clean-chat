definition module ChatCore

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

newChat         :: !String !String !Real !Bool -> ChatState
appendMessage   :: !Role !String !ChatState -> ChatState
setSystemPrompt :: !String !ChatState -> ChatState
dropLastMessage :: !ChatState -> ChatState
clearChat       :: !ChatState -> ChatState
roleName        :: !Role -> String
