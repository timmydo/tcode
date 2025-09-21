#!/bin/sh
# Start tcode in isolated mode (vendor-only dependencies)
# This will fail if any dependencies are missing from vendor/

export CL_SOURCE_REGISTRY="$(pwd)//:"
# Disable Quicklisp by clearing its config directory
export QUICKLISP_HOME=""

# unsetting home will break our terminal
#export HOME=""
#  --eval '(sb-int:set-floating-point-modes :traps nil)' \

exec sbcl --noinform --no-userinit --non-interactive \
  --eval "(require \"asdf\")" \
  --eval '(asdf:load-system :tcode)' \
  --eval '(tcode:main)'
