#!/bin/sh

CMD_READLINK="readlink"
if uname -a | grep -qE "Darwin" &> /dev/null ; then
    CMD_READLINK="greadlink"
fi
    
ln -s $($CMD_READLINK -f .bashrc)    ~/.bashrc

ln -s $($CMD_READLINK -f .emacs)     ~/.emacs
mkdir -p ~/.emacs.d
ln -s $($CMD_READLINK -f .emacs.d/custom.el) ~/.emacs.d/custom.el

ln -s $($CMD_READLINK -f .gdbinit)   ~/.gdbinit
ln -s $($CMD_READLINK -f .tmux.conf) ~/.tmux.conf
ln -s $($CMD_READLINK -f .vimrc)     ~/.vimrc
ln -s $($CMD_READLINK -f .vnc)       ~/.vnc
ln -s $($CMD_READLINK -f .zshrc)     ~/.zshrc
