#!/bin/bash

# Read library definitions:
LIBDIR=`dirname "$0"`
source "$LIBDIR/tricycle_lib.sh"

perform_command "$@"
