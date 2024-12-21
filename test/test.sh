#!/bin/sh

# Arguments should be, in order:
# $1 -> Path.
# $2 -> Expected string.
# $3 -> Optional input string.

SCRIPT="$(./chicken-chicken --global "$1");console.log(globalThis.chicken('$3'))"
OUTPUT=$(echo "$SCRIPT" | node)

[ "$2" = "$OUTPUT" ]
