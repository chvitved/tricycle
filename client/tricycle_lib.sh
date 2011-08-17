#!/bin/bash

#==================== Error handling ========================================
function usage {
    if [ $# -gt 0 ] ; then echo "$*" >&2 ; fi
    cat >&2 <<EOF
Usage:
$0 jira
    Show current Jira ticket ID
$0 jira <ticketID>
    Switch to a Jira ticket.

$0 ci
    Send current revision to Continuous Integration server.

$0 ?
    Query CI build status.

$0 resolve [<resolution>]
    Resolve Jira ticket and merge.
    Requires that the current revision was built successfully.
    <resolution> must be one of (c|w|n | closed | fixed | wontfix)

$0 config
    Print out the client's configuration.

EOF
}

function bad_usage {
    usage "$@" ; exit 1
}

#==================== Configuration ========================================
CONFIG_FILE="./.tricycle"
function read_configuration {
    if [ -f "$CONFIG_FILE" -a -r "$CONFIG_FILE" ] ; then
	echo "Reading config..."
	shopt -s extglob # Used for trimming
	while s=""; read s || [ -n "$s" ] ; do
	    if [[ "$s" == "#"* ]]  ; then continue; fi # Comment line
	    if [[ "$s" != *"="* ]] ; then continue; fi # No '='

	    propname=${s%%=*} ; propname=${propname%%*([[:space:]])}
	    propvalue=${s#*=} ; propvalue=${propvalue##*([[:space:]])}

	    varname="tmp_cfg_${propname}"
	    declare "$varname"="$propvalue"
	done < "$CONFIG_FILE"
    fi

    # Export:
    # (Why does 'declare' make variables local? -ES)
    cfg_server_host="$tmp_cfg_server_host"
}

function dump_configuration {
    echo '/---- Tricycle client configuration'
    for var in "${!cfg_*}" ; do
	printf "%-16s = %s\n" "${var#cfg_}" `eval "echo \\$$var"`
    done
    echo '\---- Tricycle client configuration'
}

#==================== Communication ========================================

#==================== Individual commands ===================================
function show_ticket_id {
    echo "<show Jira ID>" # TODO
}

function set_ticket_id {
    local id="$1"
    echo "<set Jira ID to $id>" # TODO
}

function show_build_status {
    echo "<show build status>" # TODO
}

#==================== Command-line parsing ==============================

function perform_command {
    # Special case: no command was given.
    if [ $# -eq 0 ] ; then usage ; exit 0 ; fi

    local cmd="$1"
    read_configuration

    case "$cmd" in
	jira)
	    case $# in
		1) show_ticket_id ;;
		2) set_ticket_id "$2" ;;
		*) bad_usage
	    esac
	    ;;
	config)
	    case $# in
		1) dump_configuration ;;
		*) bad_usage
	    esac
	    ;;
	"?") show_build_status ;;
	*) bad_usage "Unknown command '$cmd'." ;;
    esac
}
