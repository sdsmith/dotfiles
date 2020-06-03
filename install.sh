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
        echo "~/$FILE already exists and is not a symlink"
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

# Setup tmux with Tmux Package Manager (TPM)
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
create_home_symlink .tmux.conf
mkdir -p $HOME/.tmux.d
for tmux_file in $(find ./.tmux.d -type f); do
    create_home_symlink "$tmux_file"
done
tmux new-session -d -s "_tmux_install" "$HOME/.tmux/plugins/tpm/bin/install_plugins"

create_home_symlink .gdbinit
create_home_symlink .vimrc
create_home_symlink .vnc
create_home_symlink .zshrc
create_home_symlink .gitconfig
create_home_symlink .ptconfig.toml

# xclip
if ! command -v xclip >/dev/null; then
    echo "WARNING: Please install xclip! Needed by tmux"
fi

# FZF
if ! command -v fzf >/dev/null; then
    echo "WARNING: Please install fzf! https://github.com/junegunn/fzf"
fi

# PT
if ! command -v pt >/dev/null; then
    echo "WARNING: Please install pt! https://github.com/monochromegane/the_platinum_searcher"
fi

cd utils
make
cd ..
