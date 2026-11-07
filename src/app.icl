module app

import StdEnv
import ChatCore
from Data.Error import :: MaybeError(..), isError, fromOk
import Text.GenJSON
import System.CommandLine
import System.Environment
import System.Process
import System.SysCall
import System.File

:: CommandLineArgs =
    { argBaseUrl     :: !?String
    , argModel       :: !?String
    , argTemperature :: !?Real
    , argNoStream    :: !Bool
    , argApiKey      :: !?String
    , argHelp        :: !Bool
    }

defaultArgs :: CommandLineArgs
defaultArgs =
    { argBaseUrl     = ?None
    , argModel       = ?None
    , argTemperature = ?None
    , argNoStream    = False
    , argApiKey      = ?None
    , argHelp        = False
    }

Start :: *World -> *World
Start world
    # (argv, world) = getCommandLine world
    # args = parseArgs (if (isEmpty argv) [] (tl argv))
    # (console, world) = stdio world
    | args.argHelp
        # console = showHelp console
        = snd (fclose console world)
    # (mbEnvUrl, world) = getEnvironmentVariable "OPENAI_BASE_URL" world
    # (mbEnvModel, world) = getEnvironmentVariable "OPENAI_MODEL" world
    # (mbEnvKey, world) = getEnvironmentVariable "OPENAI_API_KEY" world
    # defaultUrl = case mbEnvUrl of ?Just u | trimWhitespace u <> "" = trimWhitespace u; _ = "https://api.openai.com/v1"
    # defaultModel = case mbEnvModel of ?Just m | trimWhitespace m <> "" = trimWhitespace m; _ = "gpt-4o-mini"
    # envKey = case mbEnvKey of ?Just k = trimWhitespace k; _ = ""
    # console = fwrites "clean-chat - OpenAI-compatible Chat Completions\n" console
    // Determine Base URL
    # (baseUrl, console) = promptBaseUrl args.argBaseUrl defaultUrl console
    # baseUrl = trimTrailingSlash (trimWhitespace baseUrl)
    // Determine Model
    # model = case args.argModel of
        ?Just m = m
        ?None = defaultModel
    // Determine API Key
    # (apiKey, console) = promptApiKey args.argApiKey envKey console
    // Determine Temperature & Streaming
    # temp = case args.argTemperature of
        ?Just t = t
        ?None = 0.7
    # isStream = not args.argNoStream
    # endpoint = baseUrl +++ "/chat/completions"
    # console = fwrites ("\nclean-chat | model: " +++ model +++ " | stream: " +++ (if isStream "true" "false") +++ "\n") console
    # console = fwrites "Commands: /exit, /quit, /clear, /system <prompt>, /model <name>, /models, /temp <val>, /stream, /help\n" console
    # console = fwrites ("Endpoint: " +++ endpoint +++ "\n\n") console
    # (_, console) = fflush console
    # state = newChat model baseUrl temp isStream
    # state = setSystemPrompt "You are a helpful assistant." state
    = chatLoop apiKey state console world

promptBaseUrl :: !(?String) !String !*File -> (!String, !*File)
promptBaseUrl (?Just u) _ console = (u, console)
promptBaseUrl ?None defaultUrl console
    # console = fwrites ("API base URL [" +++ defaultUrl +++ "]: ") console
    # (_, console) = fflush console
    # (urlIn, console) = freadline console
    # urlIn = trimWhitespace urlIn
    = (if (urlIn == "") defaultUrl urlIn, console)

promptApiKey :: !(?String) !String !*File -> (!String, !*File)
promptApiKey (?Just k) _ console = (k, console)
promptApiKey ?None envKey console
    | envKey <> "" = (envKey, console)
    # console = fwrites "OpenAI API key (leave blank if not required): " console
    # (_, console) = fflush console
    # (keyIn, console) = freadline console
    = (trimWhitespace keyIn, console)

parseArgs :: ![String] -> CommandLineArgs
parseArgs args = parseLoop args defaultArgs
where
    parseLoop [] acc = acc
    parseLoop [opt, val:rest] acc
        | opt == "-BaseUrl" || opt == "--base-url" || opt == "-url" || opt == "--url" || opt == "-u"
            = parseLoop rest { acc & argBaseUrl = ?Just val }
        | opt == "-Model" || opt == "--model" || opt == "-m"
            = parseLoop rest { acc & argModel = ?Just val }
        | opt == "-Temperature" || opt == "--temperature" || opt == "-temp" || opt == "--temp" || opt == "-t"
            = parseLoop rest { acc & argTemperature = ?Just (toReal val) }
        | opt == "-ApiKey" || opt == "--api-key" || opt == "-key" || opt == "--key" || opt == "-k"
            = parseLoop rest { acc & argApiKey = ?Just val }
    parseLoop [opt:rest] acc
        | opt == "-NoStream" || opt == "--no-stream"
            = parseLoop rest { acc & argNoStream = True }
        | opt == "-Stream" || opt == "--stream"
            = parseLoop rest { acc & argNoStream = False }
        | opt == "-Help" || opt == "--help" || opt == "-h" || opt == "/?" || opt == "-?"
            = parseLoop rest { acc & argHelp = True }
        = parseLoop rest acc

chatLoop :: !String !ChatState !*File !*World -> *World
chatLoop apiKey state console world
    # console = fwrites "you> " console
    # (_, console) = fflush console
    # (line, console) = freadline console
    | size line == 0
        # console = fwrites "\n" console
        = snd (fclose console world)
    # trimmed = trimWhitespace line
    | trimmed == "" = chatLoop apiKey state console world
    | trimmed == "/exit" || trimmed == "/quit"
        = snd (fclose console world)
    | trimmed == "/clear"
        # state = clearChat state
        # console = fwrites "Conversation cleared.\n" console
        # (_, console) = fflush console
        = chatLoop apiKey state console world
    | startsWith "/system " trimmed
        # sysPrompt = trimWhitespace (trimmed % (8, size trimmed - 1))
        # state = setSystemPrompt sysPrompt state
        # console = fwrites "System prompt updated.\n" console
        # (_, console) = fflush console
        = chatLoop apiKey state console world
    | trimmed == "/system"
        # currentSys = getSystemPrompt state
        # console = fwrites ("Current system prompt: " +++ currentSys +++ "\n") console
        # (_, console) = fflush console
        = chatLoop apiKey state console world
    | startsWith "/model " trimmed
        # newModel = trimWhitespace (trimmed % (7, size trimmed - 1))
        # state = { state & model = newModel }
        # console = fwrites ("Model: " +++ newModel +++ "\n") console
        # (_, console) = fflush console
        = chatLoop apiKey state console world
    | trimmed == "/models"
        # console = fwrites ("Fetching models from " +++ state.baseUrl +++ " ...\n") console
        # (_, console) = fflush console
        # (models, world) = fetchModels state.baseUrl apiKey world
        # console = showModels models console
        # (_, console) = fflush console
        = chatLoop apiKey state console world
    | startsWith "/temp " trimmed
        # valStr = trimWhitespace (trimmed % (6, size trimmed - 1))
        # state = { state & temperature = toReal valStr }
        # console = fwrites ("Temperature set to " +++ toString state.temperature +++ "\n") console
        # (_, console) = fflush console
        = chatLoop apiKey state console world
    | trimmed == "/stream"
        # newStream = not state.stream
        # state = { state & stream = newStream }
        # console = fwrites ("Streaming " +++ (if newStream "enabled" "disabled") +++ ".\n") console
        # (_, console) = fflush console
        = chatLoop apiKey state console world
    | trimmed == "/help"
        # console = showHelp console
        # (_, console) = fflush console
        = chatLoop apiKey state console world
    // Send user message
    # stateWithUser = appendMessage User trimmed state
    # (res, console, world) = completeChat apiKey stateWithUser console world
    = case res of
        Ok assistantText
            # stateWithAssistant = appendMessage Assistant assistantText stateWithUser
            = chatLoop apiKey stateWithAssistant console world
        Error _
            // Revert state to before user message on failure
            = chatLoop apiKey state console world

completeChat :: !String !ChatState !*File !*World -> (!MaybeError String String, !*File, !*World)
completeChat apiKey state console world
    # tempFilePath = ".clean-chat-req.json"
    # (writeErr, world) = writeFile tempFilePath (toString (requestJSON state)) world
    | isError writeErr
        # console = fwrites "Failed to write request payload.\n" console
        = (Error "Failed to write payload", console, world)
    # endpoint = state.baseUrl +++ "/chat/completions"
    | state.stream
        = streamCompletion apiKey endpoint tempFilePath console world
    = nonStreamCompletion apiKey endpoint tempFilePath console world

nonStreamCompletion :: !String !String !String !*File !*World -> (!MaybeError String String, !*File, !*World)
nonStreamCompletion apiKey endpoint tempFilePath console world
    # args = ["-sS", "-X", "POST", endpoint, "-H", "Content-Type: application/json"]
             ++ (if (apiKey == "") [] ["-H", "Authorization: Bearer " +++ apiKey])
             ++ ["--data-binary", "@" +++ tempFilePath]
    # (res, world) = callProcessWithOutput "curl.exe" args ?None world
    # (_, world) = deleteFile tempFilePath world
    = case res of
        Error (_, errMsg)
            # console = fwrites ("Request failed (OS error): " +++ errMsg +++ "\n") console
            = (Error errMsg, console, world)
        Ok pr
            | pr.ProcessResult.exitCode <> 0
                # errText = if (pr.stderr <> "") (trimWhitespace pr.stderr) ("curl exited with code " +++ toString pr.exitCode)
                # console = fwrites ("\nRequest failed: " +++ errText +++ "\n") console
                # (_, console) = fflush console
                = (Error errText, console, world)
            # json = fromString pr.ProcessResult.stdout
            = parseNonStreamResponse json pr.ProcessResult.stdout pr.ProcessResult.stderr console world

parseNonStreamResponse :: !JSONNode !String !String !*File !*World -> (!MaybeError String String, !*File, !*World)
parseNonStreamResponse json stdout stderr console world
    = case jsonQuery "choices/0/message/content" json of
        ?Just content
            # console = fwrites ("\nassistant> " +++ content +++ "\n") console
            # (_, console) = fflush console
            = (Ok content, console, world)
        ?None = case extractErrorMessage json of
            ?Just err
                # console = fwrites ("\nRequest failed: " +++ err +++ "\n") console
                # (_, console) = fflush console
                = (Error err, console, world)
            ?None
                # out = if (stdout <> "") stdout stderr
                # console = fwrites ("\nRequest failed: " +++ out +++ "\n") console
                # (_, console) = fflush console
                = (Error out, console, world)

streamCompletion :: !String !String !String !*File !*World -> (!MaybeError String String, !*File, !*World)
streamCompletion apiKey endpoint tempFilePath console world
    # args = ["-sS", "-N", "-X", "POST", endpoint, "-H", "Content-Type: application/json", "-H", "Accept: text/event-stream"]
             ++ (if (apiKey == "") [] ["-H", "Authorization: Bearer " +++ apiKey])
             ++ ["--data-binary", "@" +++ tempFilePath]
    # (mbProc, world) = runProcessIO "curl.exe" args ?None world
    = case mbProc of
        Error (_, errMsg)
            # (_, world) = deleteFile tempFilePath world
            # console = fwrites ("\nRequest failed: " +++ errMsg +++ "\n") console
            = (Error errMsg, console, world)
        Ok (ph, pio)
            # (_, world) = closePipe pio.stdIn world
            # (answer, hasError, console, world) = readSseLoop pio.stdOut "" "" False False console world
            # (_, world) = closePipe pio.stdOut world
            # (mbExitCode, world) = waitForProcess ph world
            # (mbErrOut, world) = readPipeNonBlocking pio.stdErr world
            # (_, world) = closePipe pio.stdErr world
            # (_, world) = deleteFile tempFilePath world
            # exitCode = getExitCode mbExitCode
            # stderrStr = getStderrStr mbErrOut
            | exitCode <> 0 || hasError || answer == ""
                # errDetail = if (stderrStr <> "") stderrStr (if hasError answer (if (exitCode <> 0) ("curl exited with code " +++ toString exitCode) "Empty response from server"))
                # console = if (not hasError && answer == "") (fwrites ("\nRequest failed: " +++ errDetail +++ "\n") console) console
                # console = if (answer <> "" && not hasError) (fwrites "\n" console) console
                # (_, console) = fflush console
                = (Error errDetail, console, world)
            # console = fwrites "\n" console
            # (_, console) = fflush console
            = (Ok answer, console, world)

readSseLoop :: !ReadPipe !String !String !Bool !Bool !*File !*World -> (!String, !Bool, !*File, !*World)
readSseLoop pipe accBuf accAnswer headerPrinted hasError console world
    # (mbChunk, world) = readPipeBlocking pipe world
    = case mbChunk of
        Error (_, err)
            | accAnswer == ""
                # console = fwrites ("\nRead pipe error: " +++ err +++ "\n") console
                = (err, True, console, world)
            = (accAnswer, hasError, console, world)
        Ok ""
            // EOF from child process
            # lastLines = if (trimWhitespace accBuf == "") [] [accBuf]
            # (ans, errFlag, _, _, console) = processLinesUntilDone lastLines accAnswer headerPrinted hasError console
            = (ans, errFlag, console, world)
        Ok chunk
            # (lines, remaining) = extractLines (accBuf +++ chunk)
            # (newAns, newErrFlag, isDone, newHeaderPrinted, console) = processLinesUntilDone lines accAnswer headerPrinted hasError console
            | isDone
                = (newAns, newErrFlag, console, world)
            = readSseLoop pipe remaining newAns newHeaderPrinted newErrFlag console world

processLinesUntilDone :: ![String] !String !Bool !Bool !*File -> (!String, !Bool, !Bool, !Bool, !*File)
processLinesUntilDone [] ans headerPrinted err console = (ans, err, False, headerPrinted, console)
processLinesUntilDone [line:rest] ans headerPrinted err console
    # (mbDelta, isDone, errFound, newHeaderPrinted, console) = processSingleSseLine line headerPrinted console
    # newAns = case mbDelta of ?Just d = ans +++ d; ?None = ans
    # newErr = err || errFound
    | isDone = (newAns, newErr, True, newHeaderPrinted, console)
    = processLinesUntilDone rest newAns newHeaderPrinted newErr console

processSingleSseLine :: !String !Bool !*File -> (!?String, !Bool, !Bool, !Bool, !*File)
processSingleSseLine line headerPrinted console
    # trimmed = trimWhitespace line
    | trimmed == "" || startsWith ":" trimmed
        = (?None, False, False, headerPrinted, console)
    | startsWith "data: [DONE]" trimmed || trimmed == "data:[DONE]"
        = (?None, True, False, headerPrinted, console)
    | startsWith "data:" trimmed
        # payload = trimWhitespace (if (startsWith "data: " trimmed) (trimmed % (6, size trimmed - 1)) (trimmed % (5, size trimmed - 1)))
        | payload == "[DONE]"
            = (?None, True, False, headerPrinted, console)
        # json = fromString payload
        = case jsonQuery "choices/0/delta/content" json of
            ?Just delta
                # console = if headerPrinted console (fwrites "\nassistant> " console)
                # console = fwrites delta console
                # (_, console) = fflush console
                = (?Just delta, False, False, True, console)
            ?None = case extractErrorMessage json of
                ?Just errMsg
                    # console = fwrites ("\nRequest failed: " +++ errMsg +++ "\n") console
                    # (_, console) = fflush console
                    = (?Just errMsg, True, True, headerPrinted, console)
                ?None = (?None, False, False, headerPrinted, console)
    | startsWith "{\"" trimmed || startsWith "{" trimmed
        # json = fromString trimmed
        = case extractErrorMessage json of
            ?Just errMsg
                # console = fwrites ("\nRequest failed: " +++ errMsg +++ "\n") console
                # (_, console) = fflush console
                = (?Just errMsg, True, True, headerPrinted, console)
            ?None
                # console = fwrites ("\nRequest failed: " +++ trimmed +++ "\n") console
                # (_, console) = fflush console
                = (?Just trimmed, True, True, headerPrinted, console)
    = (?None, False, False, headerPrinted, console)

extractErrorMessage :: !JSONNode -> ?String
extractErrorMessage json = case jsonQuery "error/message" json of
    ?Just msg = ?Just msg
    ?None = case jsonQuery "error" json of
        ?Just msg = ?Just msg
        ?None = jsonQuery "message" json

extractLines :: !String -> (![String], !String)
extractLines s = splitLines s 0 0 (size s) []
where
    splitLines s lastIdx idx len acc
        | idx >= len
            # remaining = if (lastIdx >= len) "" (s % (lastIdx, len - 1))
            = (reverse acc, remaining)
        | s.[idx] == '\n'
            # rawLine = if (lastIdx > idx - 1) "" (s % (lastIdx, idx - 1))
            # line = stripCarriageReturn rawLine
            = splitLines s (idx + 1) (idx + 1) len [line:acc]
        = splitLines s lastIdx (idx + 1) len acc

    stripCarriageReturn str
        # sz = size str
        | sz > 0 && str.[sz - 1] == '\r' = str % (0, sz - 2)
        = str

fetchModels :: !String !String !*World -> (![String], !*World)
fetchModels baseUrl apiKey world
    # args = ["-sS", "-X", "GET", baseUrl +++ "/models"]
             ++ (if (apiKey == "") [] ["-H", "Authorization: Bearer " +++ apiKey])
    # (res, world) = callProcessWithOutput "curl.exe" args ?None world
    = case res of
        Ok pr
            # node = fromString pr.ProcessResult.stdout
            = (modelIds node, world)
        Error _ = ([], world)

modelIds :: !JSONNode -> [String]
modelIds (JSONObject fields) = case lookupField "data" fields of
    ?Just (JSONArray items) = collectModelIds items
    _ = []
modelIds _ = []

collectModelIds :: ![JSONNode] -> [String]
collectModelIds [item:rest] = case modelId item of
    ?Just id = [id:collectModelIds rest]
    ?None = collectModelIds rest
collectModelIds [] = []

modelId :: !JSONNode -> ?String
modelId (JSONObject fields) = lookupString "id" fields
modelId _ = ?None

lookupField :: !String ![(String,JSONNode)] -> ?JSONNode
lookupField key [(name,value):rest]
    | key == name = ?Just value
    = lookupField key rest
lookupField _ [] = ?None

lookupString :: !String ![(String,JSONNode)] -> ?String
lookupString key fields = case lookupField key fields of
    ?Just (JSONString value) = ?Just value
    _ = ?None

showModels :: ![String] !*File -> *File
showModels [] console = fwrites "Could not retrieve models. Check the URL/key and use /models to retry.\n" console
showModels models console = showModelsFrom 1 models console

showModelsFrom :: !Int ![String] !*File -> *File
showModelsFrom _ [] console = console
showModelsFrom n [model:rest] console
    # console = fwrites (toString n +++ ". " +++ model +++ "\n") console
    = showModelsFrom (n+1) rest console

showHelp :: !*File -> *File
showHelp console =
    fwrites ("Commands:\n"
         +++ "  /exit, /quit          - Exit clean-chat\n"
         +++ "  /clear                - Clear conversation history and reset system prompt\n"
         +++ "  /system <prompt>      - Set system prompt\n"
         +++ "  /model <name>         - Switch active model\n"
         +++ "  /models               - List available models from the endpoint\n"
         +++ "  /temp <val>           - Set temperature (e.g. 0.7)\n"
         +++ "  /stream               - Toggle streaming mode on/off\n"
         +++ "  /help                 - Show this help\n\n"
         +++ "Command-line Options:\n"
         +++ "  -BaseUrl, --url <url> - Set API base URL\n"
         +++ "  -Model, --model <m>   - Set model name\n"
         +++ "  -Temperature, -t <r>  - Set sampling temperature\n"
         +++ "  -ApiKey, --key <k>    - Set API key\n"
         +++ "  -NoStream             - Disable streaming\n"
         +++ "  -Stream               - Enable streaming\n\n"
         +++ "Environment Variables:\n"
         +++ "  OPENAI_BASE_URL       - Default API base URL (default: https://api.openai.com/v1)\n"
         +++ "  OPENAI_MODEL          - Default model (default: gpt-4o-mini)\n"
         +++ "  OPENAI_API_KEY        - API key\n\n"
    ) console

getSystemPrompt :: !ChatState -> String
getSystemPrompt state = case state.messages of
    [{role = System, content}:_] = content
    _ = "(no system prompt)"

requestJSON :: !ChatState -> JSONNode
requestJSON state =
    JSONObject
        [ ("model", JSONString state.model)
        , ("messages", JSONArray (map messageJSON state.messages))
        , ("temperature", JSONReal state.temperature)
        , ("stream", JSONBool state.stream)
        ]

messageJSON :: !Message -> JSONNode
messageJSON message =
    JSONObject
        [ ("role", JSONString (roleName message.role))
        , ("content", JSONString message.content)
        ]

trimWhitespace :: !String -> String
trimWhitespace s = trimEnd (trimStart 0)
where
    len = size s
    trimStart i
        | i >= len = i
        | isSpace s.[i] = trimStart (i + 1)
        = i
    trimEnd start
        | start >= len = ""
        = slice start (lastNonSpace (len - 1))
    lastNonSpace i
        | i < 0 = 0
        | isSpace s.[i] = lastNonSpace (i - 1)
        = i
    slice start end
        | start > end = ""
        = s % (start, end)
    isSpace c = c == ' ' || c == '\t' || c == '\r' || c == '\n'

trimTrailingSlash :: !String -> String
trimTrailingSlash s
    # len = size s
    | len > 0 && (s.[len - 1] == '/' || s.[len - 1] == '\\')
        = trimTrailingSlash (s % (0, len - 2))
    = s

getExitCode :: !.(MaybeError e Int) -> Int
getExitCode res
    | isError res = 0
    = fromOk res

getStderrStr :: !.(MaybeError e String) -> String
getStderrStr res
    | isError res = ""
    = trimWhitespace (fromOk res)

startsWith :: !String !String -> Bool
startsWith prefix value
    # pLen = size prefix
    | size value < pLen = False
    = (value % (0, pLen - 1)) == prefix
