#!/bin/bash

# NOTE: must be called from this directory!

CMD_READLINK="readlink"
if uname -a | grep -qE "Darwin" &> /dev/null ; then
    CMD_READLINK="greadlink"
fi

DOTFILES_DIR=$($CMD_READLINK -f "$(pwd)/..")
export ZSH="$DOTFILES_DIR/oh-my-zsh"
export ZSH_CUSTOM="$ZSH/custom"


function create_oh_my_zsh_custom_symlink() {
    local FILE=$1
    local ABS_PATH=$($CMD_READLINK -f $FILE)
    local DEST="$ZSH_CUSTOM/$FILE"

    if [ -f "$DEST" ] && [ ! -h "$DEST" ]; then
        echo "$DEST already exists and in not a symlink"
        return 1
    fi

    mkdir -p "$ZSH_CUSTOM/$(dirname $FILE)"
    ln -sf "$ABS_PATH" "$DEST"
}

create_oh_my_zsh_custom_symlink plugins/mercurial/mercurial.plugin.zsh
create_oh_my_zsh_custom_symlink themes/zeta.zsh-theme
