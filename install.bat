@echo off
SETLOCAL

IF DEFINED HOME (
    SET dest=%HOME%
)ELSE (
    SET dest=%UserProfile%
)

mklink "%dest%\.bashrc"	"%~dp0.bashrc"

mklink "%dest%\.emacs"	           "%~dp0.emacs"
mklink "%dest%\.emacs.d\custom.el" "%~dp0.emacs.d\custom.el"

mklink "%dest%\.gdbinit"   "%~dp0.gdbinit"
mklink "%dest%\.tmux.conf" "%~dp0.tmux.conf"
mklink "%dest%\.vimrc"	   "%~dp0.vimrc"
mklink "%dest%\.zshrc"	   "%~dp0.zshrc"
