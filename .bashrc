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

if is_running_windows_subsystem_linux ; then
    export SW_WS_BASE_PATH=/mnt/s/branch
    export HW_WS_BASE_PATH=/mnt/r/branch

    # WAR: mount drives. WSL doesn't mount drives in current release (Jan 2019)
    . ~/scripts/mount_network_drives
else
    if is_running_cygwin ; then
	export SW_WS_BASE_PATH=/cygdrive/s/branch
	export HW_WS_BASE_PATH=/cygdrive/r/branch    
    else
	
	export SW_WS_BASE_PATH=/home/scratch.stewarts_sw/branch
	export HW_WS_BASE_PATH=/home/scratch.stewarts_hw/branch
    fi
fi

## Increase bash history limit
#export HISTSIZE=-1
#export HISTFILESIZE=-1

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

### Perforce workspace switching ###############################################

function __ws_show_help() {
cat <<EOF
Usage: ws [-Hmshc] <client>
       -H               display this help and exit
       -m               change to the MODS root directory
       -r               change to the MODS runspace directory
       -s               check for branch in the software tree, this is the
                        default
       -h               check for branch in the hardware tree
       -c               clear the P4 environment settings and exit the workspace
       -f               change to nvflash directory
       client           client to switch to
EOF
}

# Switch to mods dir in workspace
function __ws_mods_cd() {
    local BRANCH="$1"
    local PATH=""
    local MODS_DIR=""
    local DRIVER_PATH=""
    
    # Remove '_tot' and '-tot' postfixes
    if [[ "$BRANCH" =~ .+[-_]tot$ ]];
    then
        BRANCH=${BRANCH:0:${#BRANCH} - 4}
    fi
    
    # TODO: cleanup
    # Remove '-.+' postfix
    if [[ "$BRANCH" =~ .+[-].$ ]];
    then
        BRANCH=${BRANCH:0:${#BRANCH} - 2}
    fi    

    if [ $BRANCH == "chips_a" ];
    then
        DRIVER_PATH=sw/dev/gpu_drv/chips_a
        MODS_DIR=${DRIVER_PATH}/diag/mods
    elif [[ "$BRANCH" == bringup* ]]
    then
        DRIVER_PATH=sw/dev/gpu_drv/${BRANCH}
        MODS_DIR=${DRIVER_PATH}/diag/mods
    else
        DRIVER_PATH=sw/rel/gpu_drv/${BRANCH}/${BRANCH}_00
        MODS_DIR=${DRIVER_PATH}/diag/mods
    fi

    export DRIVER_PATH
    
    if [ ! -d "$MODS_DIR" ];
    then
        echo "$MODS_DIR not found"
        return 1
    fi
    cd $MODS_DIR
}

function ws() {
    CLIENT_BASE_PATH="$SW_WS_BASE_PATH"
    P4PORT=p4sw:2006

    INVALID_OPTIONS=0
    TREE_SELECTED=0
    CHANGE_TO_MODS_DIR=0
    CHANGE_TO_RUNSPACE_DIR=0

    if [ $# -eq 0 ]; then
        # No arguments provided
        # Go root of the workspace, if we are inside of one.
        if [ ! "$P4ROOT" == "" ]; then
            cd $P4ROOT
            return 0
        fi

        __ws_show_help
        return 1
    fi

    # Process args
    OPTIND=1 # Reset getopts parse position

    while getopts ":Hmrshcf" opt; do
        case "$opt" in
            H)
                __ws_show_help
                return 0
                ;;

            m)
                CHANGE_TO_MODS_DIR=1
                ;;

            r)
                CHANGE_TO_MODS_DIR=1 # First need to change to MODS dir
                CHANGE_TO_RUNSPACE_DIR=1
                ;;
            
            s)
                if [ $TREE_SELECTED -eq 1 ] ; then
                    echo "Branch tree has already been selected"
                    return 1
                fi

                TREE_SELECTED=1
                CLIENT_BASE_PATH="$SW_WS_BASE_PATH"
                P4PORT=p4sw:2006
                ;;

            h)
                if [ $TREE_SELECTED -eq 1 ] ; then
                    echo "Branch tree has already been selected"
                    return 1
                fi

                TREE_SELECTED=1
                CLIENT_BASE_PATH="$HW_WS_BASE_PATH"
                P4PORT=p4hw:2001
                ;;

            c)
                cd ~
                export P4ROOT=""
                export P4_WS_NAME=""
                export P4PORT=p4sw:2006
                return 1
                ;;
            f)
                export P4ROOT=""
                export P4_WS_NAME=""
                export P4RPORT=p4sw:2006
                cd $NVFLASH_DIR
                return 1
                ;;
            
            :)
                echo "Option -$OPTARG requires an argument." >&2
                __ws_show_help
                return 1
                ;;

            \?)
                INVALID_OPTIONS=1
                echo "Invalid option: -$OPTARG" >&2
                ;;
        esac
    done
    shift "$((OPTIND-1))" # Discard the options and sentinel

    # Fail if invalid options are provided
    if [ $INVALID_OPTIONS -eq 1 ] ; then
        __ws_show_help
        return 1
    fi

    # TODO: allow for 0 args but options so we can set the P4PORT

    # Check arg count
    if [ ! $# -eq 1 ]
    then
        echo "Missing argument: branch not specified"
        __ws_show_help
        return 1
    fi

    if [ ! -d "$CLIENT_BASE_PATH" ]
    then
        echo "Path not found: $CLIENT_BASE_PATH"
        return 1
    fi
    
    # Check if dir exists
    CLIENT_ROOT="$CLIENT_BASE_PATH/$1"
    if [ ! -d "$CLIENT_ROOT" ]
    then
        echo "$1 is not a workspace client"
        return 1
    fi

    # Change dir to client
    cd "$CLIENT_ROOT"

    # Change dir to mods
    if [ $CHANGE_TO_MODS_DIR -eq 1 ] ; then
        __ws_mods_cd $1
    fi

    # Chnage dir to runspace
    if [ $CHANGE_TO_RUNSPACE_DIR -eq 1 ] ; then
        cd runspace
    fi    

    # Set P4 environment
    export P4ROOT="$CLIENT_ROOT"
    export P4_WS_NAME="$1"
    export P4PORT
}
################################################################################

# Change bash ls directory color (from default dark blue)
export LS_COLORS=$LS_COLORS:'di=0;94:'

# Tmux attach alias with session name auto complete
alias tma='tmux attach -t $1'
# if [ -f ~/.bash_completion ]; then
#     . ~/.bash_completion
# fi

alias tml='tmux ls'

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

### Additional config files
if [ -f "${HOME}/.bashrc_nvidia" ]; then
    source "${HOME}/.bashrc_nvidia"
else
    echo "WARNING: .bashrc_nvidia: not found"
fi
