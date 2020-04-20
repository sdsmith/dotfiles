#!/bin/sh

# NOTE: Must be run in the dotfiles directory!!

DOTFILES_DIR="$(pwd)"

export ZSH="$DOTFILES_DIR/oh-my-zsh"
export ZSH_CUSTOM="$ZSH/custom"

echo "Installing oh-my-zsh..."
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" "" --unattended --keep-zshrc

echo "Upgrading oh-my-zsh..."
upgrade_oh_my_zsh

## ZSH plugins
echo "Installing oh-my-zsh plugins..."
cd $ZSH/custom/plugins

# zsh-completions
# ref: git clone https://github.com/zsh-users/zsh-completions
git clone https://github.com/zsh-users/zsh-completions

# zsh-syntax-highlighting
# ref: https://github.com/zsh-users/zsh-syntax-highlighting
# TODO:
#   - change colors
#   - change path underlining (obscures underscores in file names)
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git

cd $DOTFILES_DIR

echo "Changing default shell to zsh..."
if [ "$(basename $SHELL)" != "zsh" ]; then
    chsh -s $(which zsh)
fi;

# ref: https://github.com/powerline/fonts
echo "Installing powerline-fonts..."
POWERLINE_FONTS_DIR="powerline-fonts"
git clone https://github.com/powerline/fonts.git --depth=1 $POWERLINE_FONTS_DIR
cd $POWERLINE_FONTS_DIR
./install.sh
cd ..
rm -rf $POWERLINE_FONTS_DIR

if [ ! -f $ZSH_CUSTOM/themes/zeta.zsh-theme ]; then
    echo "Installing theme..."
    # TODO: triggers env to reload in new theme, interrupting script
    bash -c "$(curl -fsSL https://raw.githubusercontent.com/skylerlee/zeta-zsh-theme/master/scripts/install.sh)"
fi;
