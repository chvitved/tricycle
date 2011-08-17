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

function error {
    echo "$@" >&2 ; exit 2
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
    cfg_server_host=${tmp_cfg_server_host}
    cfg_server_port=${tmp_cfg_server_port:=9193}
}

function dump_configuration {
    echo '/---- Tricycle client configuration'
    for var in ${!cfg_*} ; do
	printf "%-16s = %s\n" "${var#cfg_}" `eval "echo \\$$var"`
    done
    echo '\---- Tricycle client configuration'
}

#==================== Communication ========================================
function connect_to_server {
    [ -n "$cfg_server_host" ] || error "Config error: 'server_host' not set."
    [ -n "$cfg_server_port" ] || error "Config error: 'server_port' not set."
    exec 3<> "/dev/tcp/$cfg_server_host/$cfg_server_port"
    true
}

function server_request {
    local request="$1"
    echo "Server request: '$request'" >&2
    echo "$request" >&3

    local response
    read response <&3
    echo "Server response: '$response'" >&2
    echo "$response"
}

#==================== Git stuff ===================================

function current_git_revision {
    git rev-parse --verify HEAD || error "Could not figure out current local revision :-("
}

function current_build_ID {
    echo "dummyBuildID" # TODO
}

#==================== Individual commands ===================================

function show_ticket_id {
    echo "<show Jira ID>" # TODO
}

function set_ticket_id {
    local id="$1"
    echo "<set Jira ID to $id>" # TODO
}

function start_ci_build {
    local rev=`current_git_revision`
    connect_to_server
    local resp=`server_request "Start-CI $rev"`
    echo "build started for $rev :: $resp"
    # TODO: figure out build ID, and save it
}

function show_build_status {
    local buildID=`current_build_ID`
    connect_to_server
    local resp=`server_request "CI-Build-Status $buildID"`
    echo "build status for $buildID :: $resp"
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
	ci)
	    case $# in
		1) start_ci_build ;;
		*) bad_usage
	    esac
	    ;;
	"?") show_build_status ;;
	*) bad_usage "Unknown command '$cmd'." ;;
    esac
}
