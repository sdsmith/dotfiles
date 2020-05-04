#!/bin/bash

CMD_READLINK="readlink"
if uname -a | grep -qE "Darwin" &> /dev/null ; then
    CMD_READLINK="greadlink"
fi

function create_home_symlink() {
    local FILE=$1
    local ABS_PATH=$($CMD_READLINK -f $FILE)
    local DEST="$HOME/$FILE"
    
    if [ -f "$DEST" ] && [ ! -h "$DEST" ]; then
        echo "~/$FILE already exists and in not a symlink"
        return 1
    fi

    
    
    ln -sf "$ABS_PATH" "$DEST"
}

create_home_symlink .bashrc

create_home_symlink .emacs
mkdir -p $HOME/.emacs.d
for emacs_file in $(find ./.emacs.d -type f); do
    create_home_symlink "$emacs_file"
done
# Byte compile emacs files
# TODO(stewarts): Doesn't handle require statements outside of the
# main .emacs file well...
# emacs -Q --batch --eval '(byte-compile-file "~/.emacs" 0)'
# emacs -Q --batch --eval '(byte-recompile-directory "~/.emacs.d" 0)'

create_home_symlink .gdbinit
create_home_symlink .tmux.conf
create_home_symlink .vimrc
create_home_symlink .vnc
create_home_symlink .zshrc

cd utils
make
cd ..
