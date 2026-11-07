# clean-chat

A terminal OpenAI Chat Completions client implemented in Clean.

## Features

- Native Clean implementation with real-time SSE streaming (`stream: true`) and non-streaming support.
- Full OpenAI-compatible API base URL support, including local endpoints (e.g. `http://127.0.0.1:8317/v1`) and cloud providers (`https://api.openai.com/v1`).
- Environment variable support for `OPENAI_BASE_URL`, `OPENAI_MODEL`, and `OPENAI_API_KEY`.
- Command-line parameters support (`-BaseUrl`, `-Model`, `-Temperature`, `-ApiKey`, `-NoStream`, `-Stream`, `-Help`).
- Interactive startup prompts for API base URL (with customizable default) and API key (leave blank for local servers).
- In-chat slash commands:
  - `/exit`, `/quit` — exit the client.
  - `/clear` — clear conversation history and reset system prompt.
  - `/system <prompt>` — update the system prompt.
  - `/model <name>` — switch active model.
  - `/models` — list available models from `<base-url>/models`.
  - `/temp <val>` — set temperature (default `0.7`).
  - `/stream` — toggle SSE streaming on or off.
  - `/help` — display command help.
- Multi-turn conversation history with `system`, `user`, and `assistant` messages.
- Automatic failure recovery (unsuccessful user turns are dropped so conversation state stays intact).

## Build

Requires Nitrile and network access to the Clean package registry.

```powershell
nitrile update
nitrile fetch
nitrile build
```

## Run

From the project directory:

```powershell
.\bin\clean-chat.exe -nt -nr
```

Or using Nitrile:

```powershell
nitrile do run
```

You can pass options directly on the command line:

```powershell
.\bin\clean-chat.exe -nt -nr -BaseUrl https://api.openai.com/v1 -Model gpt-4o-mini -Temperature 0.7
```

Or set environment variables:

```powershell
$env:OPENAI_API_KEY = 'sk-...'
$env:OPENAI_BASE_URL = 'https://api.openai.com/v1'
$env:OPENAI_MODEL = 'gpt-4o-mini'
nitrile do run
```
