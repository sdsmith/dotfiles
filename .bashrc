shopt -s expand_aliases

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
alias tma='tmux attach -t $*'
# if [ -f ~/.bash_completion ]; then
#     . ~/.bash_completion
# fi

alias tml='tmux ls'
alias tmk='tmux kill-session -t $*'

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

### Additional config files
if [ -f "${HOME}/.bashrc_nvidia" ]; then
    source "${HOME}/.bashrc_nvidia"
else
    echo "WARNING: .bashrc_nvidia: not found"
fi
