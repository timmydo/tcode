#!/bin/sh
# Build a self-contained tcode JSONRPC server executable
# This will create a standalone binary that can be run without SBCL

set -e

OUTPUT_FILE="${1:-tcode-server}"

export CL_SOURCE_REGISTRY="$(pwd)//:"
# Disable Quicklisp by clearing its config directory
export QUICKLISP_HOME=""

# Configure OpenSSL paths for Guix systems
if [ -d "/gnu/store" ]; then
    OPENSSL_PATH=$(guix build --no-grafts openssl | grep -v -- '-doc' | grep -v -- '-static' | head -1)
fi

echo "Building tcode server executable: $OUTPUT_FILE"

if [ -d "/gnu/store" ]; then
  sbcl --noinform --no-userinit --non-interactive \
    --eval '(sb-int:set-floating-point-modes :traps nil)' \
    --eval "(require \"asdf\")" \
    --eval "(asdf:load-system :cl+ssl/config)" \
    --eval "(cl+ssl/config:define-libssl-path \"$OPENSSL_PATH/lib/libssl.so\")" \
    --eval "(cl+ssl/config:define-libcrypto-path \"$OPENSSL_PATH/lib/libcrypto.so\")" \
    --eval '(asdf:load-system :tcode)' \
    --eval '(sb-ext:save-lisp-and-die "'$OUTPUT_FILE'" :toplevel (lambda () (tcode:server :port (let ((env-port (uiop:getenv "TCODE_JSONRPC_PORT"))) (if env-port (parse-integer env-port :junk-allowed t) 9876)))) :executable t :compression t)'
else
  sbcl --noinform --no-userinit --non-interactive \
    --eval '(sb-int:set-floating-point-modes :traps nil)' \
    --eval "(require \"asdf\")" \
    --eval '(asdf:load-system :tcode)' \
    --eval '(sb-ext:save-lisp-and-die "'$OUTPUT_FILE'" :toplevel (lambda () (tcode:server :port (let ((env-port (uiop:getenv "TCODE_JSONRPC_PORT"))) (if env-port (parse-integer env-port :junk-allowed t) 9876)))) :executable t :compression t)'
fi

echo "Build complete! Run with: ./$OUTPUT_FILE"
echo "Or set port with: TCODE_JSONRPC_PORT=9999 ./$OUTPUT_FILE"
