# JSONRPC Tool Backend

The tcode application has been refactored to support a client-server architecture where tool execution can be delegated to a separate JSONRPC server.

## Architecture

- **Client** (`tcode:client`): Webview-based UI that communicates with AI backends and optionally delegates tool execution to a JSONRPC server
- **Server** (`tcode:server`): JSONRPC server that executes registered tools

## Running the Server

Start the JSONRPC tool server:

```bash
./server.sh
```

Or with a custom port:

```bash
TCODE_JSONRPC_PORT=9999 ./server.sh
```

Or directly from Lisp:

```lisp
(tcode:server :port 9876)
```

## Running the Client

### Standalone Mode (Default)

The client can run in standalone mode without a JSONRPC server. Tools are executed locally:

```bash
./client.sh
```

### Client-Server Mode

To use the JSONRPC tool backend, set the `TCODE_USE_TOOL_BACKEND` environment variable:

```bash
TCODE_USE_TOOL_BACKEND=1 ./client.sh
```

With a custom JSONRPC port:

```bash
TCODE_USE_TOOL_BACKEND=1 TCODE_JSONRPC_PORT=9999 ./client.sh
```

## JSONRPC Protocol

The server implements JSONRPC 2.0 protocol with the following methods:

### listTools

List all registered tools.

**Request:**
```json
{
  "jsonrpc": "2.0",
  "method": "listTools",
  "id": 1
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": [
    {
      "name": "shell",
      "description": "Execute a command in a persistent POSIX shell...",
      "parameters": { ... }
    }
  ]
}
```

### executeTool

Execute a tool with given arguments.

**Request:**
```json
{
  "jsonrpc": "2.0",
  "method": "executeTool",
  "params": {
    "name": "shell",
    "arguments": {
      "command": "ls -la"
    }
  },
  "id": 2
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "result": "total 48\ndrwxr-xr-x ...\n..."
}
```

## Implementation Details

- The client stores the JSONRPC backend in the `repl-context` under the `tool-backend` slot
- When tools are called, the `execute-tool-calls` function checks if a `tool-backend` is set
- If set, tool calls are sent to the JSONRPC server via `call-tool-via-backend`
- Otherwise, tools are executed locally via `call-tool`
- Communication uses line-delimited JSON over TCP sockets

## Legacy Support

The original `start.sh` script now redirects to `client.sh` for backwards compatibility.
The `tcode:main` function still exists and calls `tcode:client` internally.
