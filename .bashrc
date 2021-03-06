shopt -s expand_aliases

export PATH=/usr/local/homebrew/bin:${PATH}

function is_running_cygwin()
{
    if uname -a | grep -qE "(CYGWIN|cygwin|Cygwin)" &> /dev/null ; then
	return 0
    else
	return 1
    fi
}

function is_running_windows_subsystem_linux()
{
    if grep -qE "(Microsoft|WSL)" /proc/version &> /dev/null ; then
        return 0
    else
        return 1
    fi
}

function is_running_macos()
{
    if "`uname -s`" == "Darwin"; then
        return 0
    else
        return 1
    fi
}

## Increase bash history limit
#export HISTSIZE=-1
#export HISTFILESIZE=-1
export HISTTIMEFORMAT="%y-%m-%d %T " # Add timestamps to bash history

export P4_WS_NAME="" # Used to track the current p4 workspace

# Allow emacs GUI colours in terminal
export TERM=xterm-256color

# Default editor
export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -t"          # opens in term
export VISUAL="emacsclient -c -a emacs" # opens in GUI mode

# Set terminal prompt
# \d - current date
# \t - current time
# \h - host name
# \# - command number
# \u - user name
# \W - current working directory (short)
# \w - current working directory (full)
red=$(tput setaf 1)
green=$(tput setaf 2)
blue=$(tput setaf 4)
reset=$(tput sgr0)
export PS1="\h \[$green\]\${P4_WS_NAME}\[$reset\] \W> "

### Alias ###
# General
alias enw="emacsclient -t"
alias l="ls --color -F"
alias p4_clean_tree="p4 clean; find . -type d -empty -delete"

ssh_remove_auth_key()
{
    if test -f $HOME/.ssh/authorized_keys; then
        if grep -v "$1" $HOME/.ssh/authorized_keys > $HOME/.ssh/tmp; then
            cat $HOME/.ssh/tmp > $HOME/.ssh/authorized_keys && rm $HOME/.ssh/tmp;
        else
            rm $HOME/.ssh/authorized_keys && rm $HOME/.ssh/tmp;
        fi;
    fi
}

p4_synced_to_cl() {
    p4 cstat ...#have | grep change | awk '$3 > x { x = $3 };END { print x }'
}

# Change bash ls directory color (from default dark blue)
export LS_COLORS=$LS_COLORS:'di=0;94:'

# Tmux attach alias with session name auto complete
#alias tma='tmux attach -t $*'
# if [ -f ~/.bash_completion ]; then
#     . ~/.bash_completion
# fi

alias tml='tmux list-session'
alias tmk='tmux kill-session -t $*'
alias tma='tmux attach -t $*'

## tmux control mode (best used with iterm2)
# create
alias tmc='tmux -CC'
# resume (and detatch from any other clients connected to session)
alias tmcresume='tmux -CC a -d'
# resume/new named session
alias tmca='tmux -CC new-session -AD -s $*'

function p4c()
{
    # $1 type of change to view (arg to 'p4 changes -s <arg>')
    MAX_NUM_DISPLAY=32

    if [ $# -eq 0 ]; then
        p4 changes -u stewarts -m $MAX_NUM_DISPLAY
    else
        p4 changes -u stewarts -m $MAX_NUM_DISPLAY -s $*
    fi
}

function fix_terminal()
{
    stty sane
    tput rs1
}

function mosh_server_killall()
{
    # Kills all mosh-servers except the last one created (so we don't kill our own server!)
    # Assuming that the last server created is the one we are using.
    kill $(ps --no-headers --sort=start_time -C mosh-server -o pid | head -n -1)
}

function remove_files_with_extension_recursive()
{
    local START_PATH=$1
    local EXTENSION=$2

    if [ $# -ne 2 ]; then
        echo "Usage: ${FUNCNAME[0]} <path> <extension>"
        return 1
    fi

    find ${START_PATH} -name "*.${EXTENSION}" -type f -delete
}

function enwgdb()
{
    # Start emacs gdb session.
    emacs -nw --eval "(gdb \"gdb --annotate=3 $*\")";
}

function kbn()
{
    # Kill processes that match the given name.
    # TODO: Why did I name it kbn??
    ps ux | grep $1 | cut -d' ' -f2 | xargs kill -9
}

function get_nvidia_gpu_driver()
{
    # Find the driver that is associated with the NVIDIA VGA device (GPU) on
    # the system.
    find /sys | grep driver.*$(lspci | grep NV | grep VGA | cut -d ' ' -f1)
}

### Additional config files
source "${HOME}/.dotfiles/bash/dates.sh"

if [ -f "${HOME}/.workdotfiles/.bashrc_work" ]; then
    source "${HOME}/.workdotfiles/.bashrc_work"
fi
