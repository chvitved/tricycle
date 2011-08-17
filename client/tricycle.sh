#!/bin/bash

# Read library definitions:
LIBDIR=`dirname "$0"`
source "$LIBDIR/tricycle_lib.sh"

# TODO: cd to git root
perform_command "$@"
