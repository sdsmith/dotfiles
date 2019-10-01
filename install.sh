#!/bin/sh

ln -s $(readlink -f .bashrc)    ~/.bashrc

ln -s $(readlink -f .emacs)             ~/.emacs
ln -s $(readlink -f .emacs.d/custom.el) ~/.emacs.d/custom.el

ln -s $(readlink -f .gdbinit)   ~/.gdbinit
ln -s $(readlink -f .tmux.conf) ~/.tmux.conf
ln -s $(readlink -f .vimrc)     ~/.vimrc
ln -s $(readlink -f .vnc)       ~/.vnc
ln -s $(readlink -f .zshrc)     ~/.zshrc
