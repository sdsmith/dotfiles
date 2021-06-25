#!/usr/bin/env bash

# NOTE(sdsmith): the `date` tool is not the smae on linux vs macos.
# macos uses: `date -v +1d ...`
# linux:      `date -d "tomorrow" ...`


# function is_running_macos()
# {
#     if "`uname -s`" == "Darwin"; then
#         return 0
#     else
#         return 1
#     fi
# }

function last_workday()
{
    # Usage: last_workday FORMAT
    #  FORMAT: the format string for `date`
    fmt="%Y-%m-%d_%a"
    if [[ "$#" != "0" ]]; then
        fmt=$1
    fi

    day_num=$(date +%w)

    offset=1
    case "$day_num" in
        1) offset=3;;
        7) offset=2;;
    esac

    date -v -${offset}d "+$fmt"
}

function next_workday()
{
    # Usage: next_workday FORMAT
    #  FORMAT: the format string for `date`
    fmt="%Y-%m-%d_%a"
    if [[ "$#" != "0" ]]; then
        fmt=$1
    fi

    day_num=$(date +%w)

    offset=1
    case "$day_num" in
        5) offset=3;;
        6) offset=2;;
    esac

    date -v +${offset}d "+$fmt"
}
