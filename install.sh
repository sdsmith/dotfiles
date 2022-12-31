#!/bin/bash

CMD_READLINK="readlink"
if uname -a | grep -qE "Darwin" &> /dev/null ; then
    CMD_READLINK="greadlink"
fi

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

    if [ -f "$DEST" ] && [ ! -h "$DEST" ]; then
        echo "$DEST already exists and is not a symlink"
        return 1
    fi

    ln -sf "$ABS_PATH" "$DEST"
}

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

create_home_symlink .bash_profile
create_home_symlink .bashrc

create_home_symlink .emacs
mkdir -p "$HOME/.emacs.d"
find ./.emacs.d -type f -print0 |
    while IFS= read -r -d '' line; do
        create_home_symlink "$line"
    done
# Byte compile emacs files
# TODO(stewarts): Doesn't handle require statements outside of the
# main .emacs file well...
# emacs -Q --batch --eval '(byte-compile-file "~/.emacs" 0)'
# emacs -Q --batch --eval '(byte-recompile-directory "~/.emacs.d" 0)'

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

    git clone "$REPO_URL" "$DEST"
    return $?
}

# Setup tmux with Tmux Package Manager (TPM)
try_git_clone https://github.com/tmux-plugins/tpm "$HOME/.tmux/plugins/tpm"
create_home_symlink .tmux.conf
mkdir -p "$HOME/.tmux.d"
find ./.tmux.d -type f -print0 |
    while IFS= read -r -d '' line; do
        create_home_symlink "$line"
    done
tmux new-session -d -s "_tmux_install" "$HOME/.tmux/plugins/tpm/bin/install_plugins"

create_home_symlink .gdbinit
create_home_symlink .vimrc
create_home_symlink .vnc
create_home_symlink .zshrc
create_root_home_symlink zsh/.p10k.zsh
create_home_symlink .ptconfig.toml
create_home_symlink .inputrc

function create_home_symlink_global_gitignore() {
    local ABS_PATH
    ABS_PATH=$($CMD_READLINK -f .gitignore_global)
    local DEST
    DEST="$HOME/.gitignore"

    if [ -f "$DEST" ] && [ ! -h "$DEST" ]; then
        echo "$DEST already exists and is not a symlink"
        return 1
    fi

    ln -sf "$ABS_PATH" "$DEST"
}

create_home_symlink .gitconfig
create_home_symlink_global_gitignore

# xclip
if ! command -v xclip >/dev/null; then
    echo "WARNING: Please install xclip! Needed by tmux"
fi

# FZF
if ! command -v fzf >/dev/null; then
    # TODO(sdsmith): This doesn't seem to install the executable. I'm not sure why.
    # Fall back on the package manager. Use `apt-cache show fzf` to see how to enable
    # key bindings and auto-completion.
    echo "WARNING: Please install fzf!"

    # git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
    # pushd .
    # cd ~/.fzf
    # ./install --key-bindings --completion --no-update-rc
    # popd

fi

# PT
if ! command -v pt >/dev/null; then
    echo "Installing pt..."
    # Support both mac and linux (ref: https://unix.stackexchange.com/questions/30091/fix-or-alternative-for-mktemp-in-os-x)
    tmp_dir=$(mktemp -d 2>/dev/null || mktemp -d -t 'tmp_dir')
    curl -s "https://api.github.com/repos/monochromegane/the_platinum_searcher/releases/latest" | grep browser_download_url | grep linux_amd64 | cut -d '"' -f 4 | wget -qi - -P "$tmp_dir"
    tar -xf "$tmp_dir/pt_linux_amd64.tar.gz" -C "$tmp_dir"
    mkdir -p "$HOME/.local/bin"
    mv "$tmp_dir/pt_linux_amd64/pt" "$HOME/.local/bin"
    rm -r "$tmp_dir"
fi

# Install fonts
echo "Installing fonts..."
fonts_dir="$HOME/.local/share/fonts"
mkdir -p "$fonts_dir"
tar -xf fonts/liberation-mono.tar.gz -C "$fonts_dir"
tar -xf fonts/meslolgs.tar.gz -C "$fonts_dir"

# Make utilities
if ! command -v make >/dev/null; then
    echo "ERROR: Please install make!"
    exit 1
fi
(
    cd utils || exit
    make
)
