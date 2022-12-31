#!/bin/bash

#
# ZSH setup script
#

# NOTE: Must be run in the dotfiles directory!!

source ./install_helper.sh

DOTFILES_DIR="$(pwd)"

export ZSH="$DOTFILES_DIR/oh-my-zsh"
export ZSH_CUSTOM="$ZSH/custom"

echo "Setting up dotfiles in $HOME..."
./install.sh

if ! command -v zsh >/dev/null; then
    echo "ERROR: Please install zsh!"
    exit 1
fi

echo "Installing oh-my-zsh..."
if [ ! -d "$ZSH" ]; then
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" "" --unattended --keep-zshrc
else
    echo "ZSH directory $ZSH already exists, skipping"
fi

## ZSH plugins
echo "Installing oh-my-zsh plugins..."
cd "$ZSH/custom/plugins" || exit

# zsh-completions
try_git_clone https://github.com/zsh-users/zsh-completions

# zsh-syntax-highlighting
# ref: https://github.com/zsh-users/zsh-syntax-highlighting
# TODO:
#   - change colors
#   - change path underlining (obscures underscores in file names)
try_git_clone https://github.com/zsh-users/zsh-syntax-highlighting.git

cd "$DOTFILES_DIR" || exit

echo "Changing default shell to zsh..."
if [ "$(basename "$SHELL")" != "zsh" ]; then
    chsh -s "$(command -v zsh)"
fi;

INSTALL_CACHE_DIR="./install_cache"
mkdir -p "$INSTALL_CACHE_DIR"

# ref: https://github.com/powerline/fonts
echo "Installing powerline-fonts..."
POWERLINE_FONTS_DIR="$INSTALL_CACHE_DIR/powerline-fonts"
try_git_clone https://github.com/powerline/fonts.git $POWERLINE_FONTS_DIR
(
    cd $POWERLINE_FONTS_DIR || exit
    ./install.sh
)

echo "Creating utils..."
if ! command -v make >/dev/null; then
    echo "ERROR: Please install make!"
    exit 1
fi
(
    cd utils || exit
    make BUILD=release
)

if [ ! -f "$ZSH/themes/zeta.zsh-theme" ]; then
    echo "Installing zeta theme..."
    # TODO: The Zeta theme installer modifies the symlink in home. Do a pull
    # request to add a --keep-zshrc arg. https://github.com/skylerlee/zeta-zsh-theme
    curl -fsSL https://raw.githubusercontent.com/skylerlee/zeta-zsh-theme/master/zeta.zsh-theme > "$ZSH/themes/zeta.zsh-theme"
fi;

if [ ! -f "$ZSH_CUSTOM/themes/powerlevel10k" ]; then
    echo "Installing powerline10k theme..."
    # Powerline 10k
    try_git_clone https://github.com/romkatv/powerlevel10k.git "${ZSH_CUSTOM}/themes/powerlevel10k"
fi

echo "Installing custom oh-my-zsh configs..."
(
    cd oh-my-zsh-custom || exit
    ./install.sh
)

echo "To updating oh-my-zsh, run 'omz update'"

echo "Done. Starting new shell..."

env zsh -l # login shell
