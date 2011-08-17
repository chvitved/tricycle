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

EOF
}

function bad_usage {
    usage "$@" ; exit 1
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
    local cmd="$1"
    case "$cmd" in
	jira)
	    case $# in
		1) show_ticket_id ;;
		2) set_ticket_id "$2" ;;
		*) bad_usage
	    esac
	    ;;
	"?") show_build_status ;;
	*) bad_usage "Unknown command '$cmd'." ;;
    esac
}
