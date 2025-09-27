#!/bin/sh
# Compile and test tcode in isolated mode (vendor-only dependencies)
# This will show compilation messages and exit without starting PTY operations
# This will fail if any dependencies are missing from vendor/

export CL_SOURCE_REGISTRY="$(pwd)//:"
# Disable Quicklisp by clearing its config directory
export QUICKLISP_HOME=""

# Configure OpenSSL paths for Guix systems
if [ -d "/gnu/store" ]; then
    OPENSSL_PATH=$(guix build --no-grafts openssl | grep -v -- '-doc' | grep -v -- '-static' | head -1)
fi

# unsetting home will break our terminal
#export HOME=""

if [ -d "/gnu/store" ]; then
  exec sbcl --noinform --no-userinit \
    --eval '(sb-int:set-floating-point-modes :traps nil)' \
    --eval "(require \"asdf\")" \
    --eval "(asdf:load-system :cl+ssl/config)" \
    --eval "(cl+ssl/config:define-libssl-path \"$OPENSSL_PATH/lib/libssl.so\")" \
    --eval "(cl+ssl/config:define-libcrypto-path \"$OPENSSL_PATH/lib/libcrypto.so\")" \
    --eval '(asdf:load-system :tcode)' \
    --eval '(tcode:main t)' \
    --quit
else
  exec sbcl --noinform --no-userinit \
    --eval '(sb-int:set-floating-point-modes :traps nil)' \
    --eval "(require \"asdf\")" \
    --eval '(asdf:load-system :tcode)' \
    --eval '(tcode:main t)' \
    --quit
fi