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
	#echo "DB| Reading config..."
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

function cd_to_git_repo_root {
    local org_dir=`pwd`
    local dir="$org_dir"
    while ! [ -d "$dir/.git" ] ; do
	local dir2=`dirname "$dir"`
	if [[ "$dir" == "$dir2" ]] ; then
	    error "Not in a Git repo...  sorry, I can't work like this."
	fi
	dir="$dir2"
    done
    #echo "DB| Git repo root is '$dir'"
    cd "$dir"
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
    send_server_request "$request"
    read_server_response
}

function send_server_request {
    local request="$1"
    #echo "DB| Server request: '$request'" >&2
    echo "$request" >&3
}

function read_server_response {
    local response
    read response <&3
    echo "Server response: '$response'" >&2
    echo "$response"
}

#==================== Local state handling ===================================
TICKET_ID_FILE="./.tricycle-ticket"
BUILD_ID_FILE="./.tricycle-build"

function get_local {
    local file="$1"
    cat "$file" 2>/dev/null
}

function set_local {
    local file="$1" value="$2"
    echo "$value" > "$file"
}

function get_local_ticketID {
    get_local "$TICKET_ID_FILE"
}

function set_local_ticketID {
    local ticketID="$1"
    set_local "$TICKET_ID_FILE" "$ticketID"
}

function get_local_buildID {
    get_local "$BUILD_ID_FILE"
}

function set_local_buildID {
    local buildID="$1"
    set_local "$BUILD_ID_FILE" "$buildID"
}

#==================== Git stuff ===================================

function current_git_revision {
    git rev-parse --verify HEAD || error "Could not figure out current local revision :-("
}

function get_project_name {
    echo "tricycle" # TODO
}

#==================== Individual commands ===================================

function show_ticket_id {
    local ticketID=`get_local_ticketID`
    echo "Ticket ID is $ticketID"
}

function set_ticket_id {
    local id="$1"
    echo "<set Jira ID to $id>"
    connect_to_server
    local resp=`server_request "Get-Ticket-Name $id"`
    echo "ticketID set :: $resp" # TODO
    # TODO: error handling

    set_local_ticketID "$id"
}

function start_ci_build {
    local rev=`current_git_revision`
    local project_name=`get_project_name`

    connect_to_server
    send_server_request "Start-CI $project_name $rev"

    local status result
    read status result <&3
    case "$status" in
	OK)
	    # Parse result:
	    local buildID link
	    read buildID link < <( echo "$result" )

	    # Actions:
	    set_local_buildID "$buildID"
	    echo "Build started for $rev ."
	    echo "Build status URL: $link"
	    ;;
	*)
	    echo "Failed: $status $buildID $link"
    esac
}

function show_build_status {
    local buildID=`get_local_buildID`
    local project_name=`get_project_name`

    connect_to_server
    send_server_request "CI-Build-Status $project_name $buildID"

    local status result
    read status result <&3
    case "$status" in
	OK)
	    # Parse result:
	    local buildStatus link
	    read buildStatus link < <( echo "$result" )

	    # Actions:
	    echo -n "Build status for #$buildID: "
	    case "$buildStatus" in
		404) echo "Not started";;
		202) echo "Build in progress  ($link)";;
		200) echo "SUCCESS";;
		400) echo "FAILURE";;
		*)   echo "Unknown: $result"
		esac
	    ;;
	*)
	    echo "Failed: $status $result"
    esac
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
