#!/bin/sh

# NOTE: Must be run in the dotfiles directory

DOTFILES_LOCATION="$HOME/dotfiles"

export ZSH="$DOTFILES_LOCATION/oh-my-zsh"
export ZSH_CUSTOM="$ZSH/custom"

echo "Installing oh-my-zsh..."
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" "" --unattended

echo "Upgrading oh-my-zsh..."
upgrade_oh_my_zsh

# ref: https://github.com/powerline/fonts
echo "Installing powerline-fonts..."
POWERLINE_FONTS_DIR="powerline-fonts"
git clone https://github.com/powerline/fonts.git --depth=1 $POWERLINE_FONTS_DIR
cd $POWERLINE_FONTS_DIR
./install.sh
cd ..
rm -rf $POWERLINE_FONTS_DIR

echo "Installing themes..."
bash -c "$(curl -fsSL https://raw.githubusercontent.com/skylerlee/zeta-zsh-theme/master/scripts/install.sh)"

# ZSH plugins
#zsh-completions
#zsh-syntax-highlighting

echo "Changing default shell to zsh..."
if [ "$0" != "zsh" ]; then
    chsh -s $(which zsh)
fi;

echo "Done."
