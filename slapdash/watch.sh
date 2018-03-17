#!/bin/bash
set -e

export FILENAME="$1"
watch \
    --differences \
    --errexit \
    'set -e; notifywait "$FILENAME" >/dev/null; gtimeout 2 stack exec slapdash-exe "$FILENAME"'
