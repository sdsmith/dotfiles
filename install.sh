#!/bin/sh

ln -s $(readlink -f .bashrc)    ~/.bashrc
ln -s $(readlink -f .bashrc)    ~/.bash_aliases

ln -s $(readlink -f .emacs)             ~/.emacs
mkdir -p ~/.emacs.d
ln -s $(readlink -f .emacs.d/custom.el) ~/.emacs.d/custom.el

ln -s $(readlink -f .gdbinit)   ~/.gdbinit
ln -s $(readlink -f .tmux.conf) ~/.tmux.conf
ln -s $(readlink -f .vimrc)     ~/.vimrc
ln -s $(readlink -f .zshrc)     ~/.zshrc
