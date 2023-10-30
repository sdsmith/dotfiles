#!/bin/bash

#
# Bash setup script
#

source ./install_helper.sh

function create_home_symlink_global_gitignore() {
    local ABS_PATH
    ABS_PATH=$($CMD_READLINK -f .gitignore_global)
    local DEST
    DEST="$HOME/.gitignore"

    create_symlink "$ABS_PATH" "$DEST"
}

echo "Checking for package dependencies..."

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

# emacs
if ! command -v emacs >/dev/null; then
    echo "WARNING: Please install emacs!"
fi

if ! command -v make >/dev/null; then
    echo "ERROR: Please install make!"
    exit 1
fi

echo "Installing dotfiles..."

create_home_symlink .bash_profile
create_home_symlink .bashrc
create_home_symlink .gdbinit
create_home_symlink .vimrc
create_home_symlink .vnc
create_home_symlink .zshrc
create_root_home_symlink zsh/.p10k.zsh
create_home_symlink .ptconfig.toml
create_home_symlink .inputrc
create_home_symlink .gitconfig
create_home_symlink_global_gitignore

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

# Setup tmux with Tmux Package Manager (TPM)
echo "Setting up tmux..."
try_git_clone https://github.com/tmux-plugins/tpm "$HOME/.tmux/plugins/tpm"
create_home_symlink .tmux.conf
mkdir -p "$HOME/.tmux.d"
if [ -d "./.tmux.d" ]; then
    find "./.tmux.d" -type f -print0 |
        while IFS= read -r -d '' line; do
            create_home_symlink "$line"
        done
fi
tmux new-session -d -s "_tmux_install" "$HOME/.tmux/plugins/tpm/bin/install_plugins"

# Install fonts
echo "Installing fonts..."
fonts_dir="$HOME/.local/share/fonts"
mkdir -p "$fonts_dir"
tar -xf fonts/liberation-mono.tar.gz -C "$fonts_dir"
tar -xf fonts/meslolgs.tar.gz -C "$fonts_dir"


echo "Building optional utilities..."
(
    cd utils || exit
    build_log=$(mktemp "/tmp/dotfiles_install_build.XXXXXX")
    make > "$build_log" 2>&1
    if [ ! $? -eq 0 ]; then
        echo "WARNING: failed to compile optional utilities, log: $build_log"
    fi
)
