DOTFILES_LOCATION="$HOME/dotfiles"

ZSH_INSTALL_SCRIPT=zsh_install_script.sh
curl -Lo $ZSH_INSTALL_SCRIPT https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh
chmod 777 $ZSH_INSTALL_SCRIPT
ZSH="$DOTFILES_LOCATION/oh-my-zsh" sh $ZSH_INSTALL_SCRIPT
upgrade_oh_my_zsh

# TODO: supposed to restart the terminal, is that required?

# TODO: install location
git clone https://github.com/powerline/fonts.git
cd fonts
./install.sh

# Theme
bash -c "$(curl -fsSL https://raw.githubusercontent.com/skylerlee/zeta-zsh-theme/master/scripts/install.sh)"

# ZSH plugins
#zsh-completions
#zsh-syntax-highlighting
