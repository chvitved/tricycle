#!/bin/bash

# Read library definitions:
LIBDIR=`dirname "$0"`
source "$LIBDIR/tricycle_lib.sh"

cd_to_git_repo_root
perform_command "$@"
