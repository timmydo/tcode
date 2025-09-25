#!/bin/sh

# Find the most recent log file in ~/.tcode/logs/ and print its contents
LOG_DIR="$HOME/.tcode/logs"

if [ ! -d "$LOG_DIR" ]; then
    echo "Error: Log directory $LOG_DIR does not exist"
    exit 1
fi

# Find the most recent file in the logs directory
RECENT_LOG=$(ls -t "$LOG_DIR"/*.log "$LOG_DIR"/*.txt "$LOG_DIR"/*log* 2>/dev/null | head -n 1)

if [ -z "$RECENT_LOG" ]; then
    echo "No log files found in $LOG_DIR"
    exit 1
fi

echo "=== Most recent log file: $RECENT_LOG ==="
cat "$RECENT_LOG"
