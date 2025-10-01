#!/bin/sh
# Start tcode client (legacy script - use client.sh instead)
# This will fail if any dependencies are missing from vendor/

exec "$(dirname "$0")/client.sh"
