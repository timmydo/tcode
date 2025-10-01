#!/bin/sh

# Find and display tcode log files
# Usage: ./log.sh [client|server]
#   client - show most recent client log
#   server - show most recent server log
#   (no arg) - show most recent log of any type

LOG_DIR="$HOME/.tcode/logs"

if [ ! -d "$LOG_DIR" ]; then
    echo "Error: Log directory $LOG_DIR does not exist"
    exit 1
fi

TYPE="$1"

case "$TYPE" in
    client)
        PATTERN="$LOG_DIR/tcode-client-*.log"
        RECENT_LOG=$(ls -t $PATTERN 2>/dev/null | head -n 1)
        if [ -z "$RECENT_LOG" ]; then
            echo "No client log files found in $LOG_DIR"
            exit 1
        fi
        ;;
    server)
        PATTERN="$LOG_DIR/tcode-server-*.log"
        RECENT_LOG=$(ls -t $PATTERN 2>/dev/null | head -n 1)
        if [ -z "$RECENT_LOG" ]; then
            echo "No server log files found in $LOG_DIR"
            exit 1
        fi
        ;;
    *)
        # Find the most recent file of any type
        RECENT_LOG=$(ls -t "$LOG_DIR"/*.log 2>/dev/null | head -n 1)
        if [ -z "$RECENT_LOG" ]; then
            echo "No log files found in $LOG_DIR"
            exit 1
        fi
        ;;
esac

echo "=== Most recent log file: $RECENT_LOG ==="
cat "$RECENT_LOG"
