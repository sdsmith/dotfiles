#!/bin/bash

#
# Bash install helper functions
#

CMD_READLINK="readlink"
if uname -a | grep -qE "Darwin" &> /dev/null ; then
    CMD_READLINK="greadlink"
fi

function create_home_symlink() {
    # Create a symlink starting fome HOME that mimics the given file's path structure.
    local FILE="$1"
    local ABS_PATH
    ABS_PATH=$($CMD_READLINK -f "$FILE")
    local DEST="$HOME/$FILE"

    if [ -e "$DEST" ] && [ ! "$(stat -L -c %d:%i "$DEST")" = "$(stat -L -c %d:%i "$ABS_PATH")" ]; then
        echo "WARNING: $DEST already exists and does not point to the right file"
        return 1
    fi

    # Create parent dir then the symlink
    mkdir -p "$(dirname "$DEST")" && ln -sf "$ABS_PATH" "$DEST"

    return $?
}

function create_root_home_symlink() {
    # Create a symlink in HOME that ignores the given file's path structure.
    local HOME_FILE
    local FILE
    local DEST
    local ABS_PATH
    HOME_FILE=$(basename "$1")
    FILE="$1"
    ABS_PATH=$($CMD_READLINK -f "$FILE")
    DEST="$HOME/$HOME_FILE"

    create_home_symlink "$ABS_PATH" "$DEST"
    return $?
}

function try_git_clone() {
    local REPO_URL="$1"
    local DEST="$2"

    # If already cloned, skip
    if [ -e "$DEST" ]; then
        if [[ -d "$DEST" && -d "$DEST/.git" ]]; then
            echo "Repo $REPO_URL already cloned to $DEST, skipping"
            return 0
        else
            echo "Failed to clone $REPO_URL: $DEST already exists and is not a git repo"
            return 1
        fi
    fi

    if [ $DEST ]; then
        git clone "$REPO_URL" --depth=1 "$DEST"
    else
        git clone "$REPO_URL" --depth=1
    fi

    return $?
}
