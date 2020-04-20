#!/bin/bash

CMD_READLINK="readlink"
if uname -a | grep -qE "Darwin" &> /dev/null ; then
    CMD_READLINK="greadlink"
fi
    
ln -s $($CMD_READLINK -f .bashrc)    $HOME/.bashrc

ln -s $($CMD_READLINK -f .emacs)     $HOME/.emacs
mkdir -p $HOME/.emacs.d
ln -s $($CMD_READLINK -f .emacs.d/custom.el) $HOME/.emacs.d/custom.el

ln -s $($CMD_READLINK -f .gdbinit)   $HOME/.gdbinit
ln -s $($CMD_READLINK -f .tmux.conf) $HOME/.tmux.conf
ln -s $($CMD_READLINK -f .vimrc)     $HOME/.vimrc
ln -s $($CMD_READLINK -f .vnc)       $HOME/.vnc
ln -s $($CMD_READLINK -f .zshrc)     $HOME/.zshrc
