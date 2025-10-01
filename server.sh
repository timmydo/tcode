#!/bin/sh
# Start tcode JSONRPC tool server
# This will fail if any dependencies are missing from vendor/

export CL_SOURCE_REGISTRY="$(pwd)//:"
# Disable Quicklisp by clearing its config directory
export QUICKLISP_HOME=""

# Configure OpenSSL paths for Guix systems
if [ -d "/gnu/store" ]; then
    OPENSSL_PATH=$(guix build --no-grafts openssl | grep -v -- '-doc' | grep -v -- '-static' | head -1)
fi

# Get port from environment variable or use default
PORT=${TCODE_JSONRPC_PORT:-9876}

if [ -d "/gnu/store" ]; then
  exec sbcl --noinform --no-userinit --non-interactive \
    --eval '(sb-int:set-floating-point-modes :traps nil)' \
    --eval "(require \"asdf\")" \
    --eval "(asdf:load-system :cl+ssl/config)" \
    --eval "(cl+ssl/config:define-libssl-path \"$OPENSSL_PATH/lib/libssl.so\")" \
    --eval "(cl+ssl/config:define-libcrypto-path \"$OPENSSL_PATH/lib/libcrypto.so\")" \
    --eval '(asdf:load-system :tcode)' \
    --eval "(tcode:server :port $PORT)"
else
  exec sbcl --noinform --no-userinit --non-interactive \
    --eval '(sb-int:set-floating-point-modes :traps nil)' \
    --eval "(require \"asdf\")" \
    --eval '(asdf:load-system :tcode)' \
    --eval "(tcode:server :port $PORT)"
fi
