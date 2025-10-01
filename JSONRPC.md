# JSONRPC Tool Backend

The tcode application uses a client-server architecture where tool execution is delegated to a JSONRPC server.

## Architecture

- **Client** (`tcode:client`): Webview-based UI that communicates with AI backends and delegates tool execution to a JSONRPC server
- **Server** (`tcode:server`): JSONRPC server that executes registered tools

The client automatically starts the JSONRPC server in a background thread, so you only need to run the client.

## Running the Client

Simply run the client script:

```bash
./client.sh
```

Or use the legacy start.sh:

```bash
./start.sh
```

The client will:
1. Start a JSONRPC server on port 9876 (or `$TCODE_JSONRPC_PORT`)
2. Connect to it for tool execution
3. Start the webview interface

### Custom JSONRPC Port

To use a different port for the JSONRPC server:

```bash
TCODE_JSONRPC_PORT=9999 ./client.sh
```

## Running the Server Standalone

If you want to run the JSONRPC server separately (for debugging or remote access):

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

## Logging

The client and server maintain separate log files:

- **Client logs**: `~/.tcode/logs/tcode-client-TIMESTAMP.log`
- **Server logs**: `~/.tcode/logs/tcode-server-TIMESTAMP.log`

To view logs:

```bash
# View most recent client log
./log.sh client

# View most recent server log
./log.sh server

# View most recent log of either type
./log.sh
```

## Tool Execution Flow

1. User sends command to client
2. Client forwards to AI backend (e.g., OpenRouter)
3. AI backend responds with tool calls
4. Client automatically sends tool calls to JSONRPC server
5. JSONRPC server executes tools and returns results
6. Client sends results back to AI backend for continuation
7. AI backend provides final response to user

Tool calls are auto-executed - no approval required.

## Implementation Details

- The client stores the JSONRPC backend in the `repl-context` under the `tool-backend` slot
- All tool calls are sent to the JSONRPC server via `call-tool-via-backend`
- Communication uses line-delimited JSON over TCP sockets
- The server runs in a background thread started by the client

## Legacy Support

The original `start.sh` script now redirects to `client.sh` for backwards compatibility.
The `tcode:main` function still exists and calls `tcode:client` internally.
