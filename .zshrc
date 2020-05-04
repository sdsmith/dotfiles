# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# To enable shell profiling, load the zprof module.
# Check results by running `zprof` when the module is enabled.
# Cannot stop shell profiling while module is laoded.
#zmodload zsh/zprof

export TERM=xterm-256color

export DOTFILES="$HOME/.dotfiles"
export DOTFILES_UTILS="$DOTFILES/utils"

# Path to your oh-my-zsh installation.
export ZSH="$DOTFILES/oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="zeta"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in ~/.oh-my-zsh/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to automatically update without prompting.
# DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS=true

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
    git
    mercurial
    ssh-agent
    zsh-completions
    zsh-syntax-highlighting
)

# oh-my-zsh ssh-agent plugin
# ref: https://github.com/ohmyzsh/ohmyzsh/tree/master/plugins/ssh-agent
# TODO: Get permissions issue when it runs find. why? It is running as me.
#_SSH_PRIVATE_KEYS=find $HOME/.ssh -type f -name "*.pub" | grep -o "[^/]*$" | sed 's/\.[^.]*$//' | xargs
#zstyle :omz:plugins:ssh-agent identities $_SSH_PRIVATE_KEYS
#unset _SSH_PRIVATE_KEYS

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

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

alias emacsserver="emacs --daemon"
alias enw="emacsclient -t"
alias l="ls --color -F"

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

# Allow emacs GUI colours in terminal
export TERM=xterm-256color

export EDITOR="emacsclient -t"          # opens in term
export VISUAL="emacsclient -c -a emacs" # opens in GUI mode

# zsh-completions
autoload -U compinit && compinit

# zsh-syntax-highlighting
source $DOTFILES/oh-my-zsh/custom/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Work config
if [ -f "$HOME/.workdotfiles/.zshrc" ]; then
    source "$HOME/.workdotfiles/.zshrc"
fi
