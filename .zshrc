# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# To enable shell profiling, load the zprof module.
# Check results by running `zprof` when the module is enabled.
# Cannot stop shell profiling while module is laoded.
#zmodload zsh/zprof

# Set default file permissions
umask 022

export TERM=xterm-256color

export DOTFILES="$HOME/.dotfiles"
export DOTFILES_UTILS="$DOTFILES/utils"

export PATH=$HOME/.local/bin:$HOME/.homebrew/bin:$DOTFILES/zsh/:$PATH

export EMAIL_PERSONAL="stewart.dryden.smith@gmail.com"

# Path to your oh-my-zsh installation.
export ZSH="$DOTFILES/oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
#ZSH_THEME="zeta"
ZSH_THEME="powerlevel10k/powerlevel10k"

HISTFILE="$HOME/.zhistory"
HISTSIZE=10000000
SAVEHIST=10000000

setopt EXTENDED_HISTORY          # Write the history file in the ":start:elapsed;command" format.
setopt INC_APPEND_HISTORY        # Write to the history file immediately, not when the shell exits.
setopt SHARE_HISTORY             # Share history between all sessions.
setopt HIST_EXPIRE_DUPS_FIRST    # Expire duplicate entries first when trimming history.
setopt HIST_IGNORE_DUPS          # Don't record an entry that was just recorded again.
setopt HIST_IGNORE_ALL_DUPS      # Delete old recorded entry if new entry is a duplicate.
setopt HIST_FIND_NO_DUPS         # Do not display a line previously found.
setopt HIST_IGNORE_SPACE         # Don't record an entry starting with a space.
setopt HIST_SAVE_NO_DUPS         # Don't write duplicate entries in the history file.
setopt HIST_REDUCE_BLANKS        # Remove superfluous blanks before recording entry.

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

# # oh-my-zsh ssh-agent plugin
# # ref: https://github.com/ohmyzsh/ohmyzsh/tree/master/plugins/ssh-agent
# # TODO(sdsmith): This gets the right keys, but says the file doesn't exist even though it does??
# _SSH_PRIVATE_KEYS=$(find ~/.ssh -regex '.*id_[^\.]*$' -type f -printf "%f\n" | tr "\n" " ")
# zstyle :omz:plugins:ssh-agent identities $_SSH_PRIVATE_KEYS
# unset _SSH_PRIVATE_KEYS

plugins=(
    git
    mercurial
    ssh-agent
    zsh-completions
    zsh-syntax-highlighting
)

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

function is_platform_cygwin()
{
    if uname -a | grep -qE "(CYGWIN|cygwin|Cygwin)" &> /dev/null ; then
        return 0
    else
        return 1
    fi
}

function get_linux_distro()
{
    # Most major distros are moving toward using /etc/os-release
    echo `awk -F= '/^NAME/{print $2}' /etc/os-release`
}

function is_os_ubuntu()
{
    if [[ `get_linux_distro` == "Ubuntu" ]]; then
        return 1
    else
        return 0
    fi
}

function cygwin_pkg_installed()
{
    if [ $# -ne 1 ]; then
        echo "Usage: cygwin_pkg_installed <pkg_name>"
    fi

    # bash ANSI C quoting:
    # https://www.gnu.org/software/bash/manual/html_node/ANSI_002dC-Quoting.html

    local pkg_name=$1
    if [[ ! -z `cygcheck -c -d ${pkg_name} | cut -d$'\n' -f 3` ]]; then
        return 0
    else
        return 1
    fi
}

function is_platform_wsl()
{
    if grep -qE "(Microsoft|WSL)" /proc/version &> /dev/null ; then
        return 0
    else
        return 1
    fi
}

function is_platform_wsl_v1() {
    # ref: https://askubuntu.com/questions/1177729/wsl-am-i-running-version-1-or-version-2
    local KERNEL_VER=$(uname -r)
    local MAJOR_VER=$(echo $KERNEL_VER | cut -d. -f1)
    local MINOR_VER=$(echo $KERNEL_VER | cut -d. -f2)

    if (( $MAJOR_VER <= 4 )) && (( $MINOR_VER < 19 )) ; then
        return 0
    else
        return 1
    fi
}

if is_platform_wsl ; then
    # Forward graphical applications to Windows xserver
    # ref: https://wiki.ubuntu.com/WSL

    if is_platform_wsl_v1 ; then
        export DISPLAY=:0
    else
        # WSL2
        export DISPLAY=$(awk '/nameserver / {print $2; exit}' /etc/resolv.conf 2>/dev/null):0
    fi
    export LIBGL_ALWAYS_INDIRECT=1

    # WSL aliases
fi

function o()
{
    local FILEPATH=$1
    # Opens a file
    if is_platform_wsl ; then
        explorer.exe `wslpath -aw ${FILEPATH}`
    else
        echo "Unsupported terminal/OS combo"
        return 1
    fi
}

alias emacsserver="emacs --daemon"
alias enw="emacsclient -a='' -t"
alias l="ls --color -F"

function ssh_gen_key()
{
    ssh-keygen -t rsa -b 4096 -C $EMAIL_PERSONAL
}

function ssh_add_key()
{
    eval $(ssh-agent -s)
    ssh-add $*
}

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

function cron_log()
{
    grep CRON /var/log/syslog
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

function color_palette()
{
    for i in {0..255}; do
        printf "\x1b[38;5;${i}mcolour${i}\x1b[0m\n"
    done
}

function kill_proc_on_port()
{
    if [ $# -ne 1 ]; then
        echo "Usage: kill_proc_on_port <port>"
    fi

    local port=$1
    kill -9 $(lsof -n -i | grep ${port} | awk '{print $2}')
}

# Allow emacs GUI colours in terminal
export TERM=xterm-256color

export EDITOR="emacsclient -t"          # opens in term
export VISUAL="emacsclient -c -a emacs" # opens in GUI mode

# zsh-completions
autoload -U compinit && compinit

# zsh-syntax-highlighting
source $DOTFILES/oh-my-zsh/custom/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

alias tmux_reload_config='tmux source-file ~/.tmux.conf'
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

# Work config
if [ -f "$HOME/.workdotfiles/.zshrc" ]; then
    source "$HOME/.workdotfiles/.zshrc"
fi

# FZF
if is_platform_cygwin; then
    # Check that packages exist
    # TODO(sdsmith): this should go into install.sh
    if [[ `cygwin_pkg_installed fzf-zsh` && `cygwin_pkg_installed fzf-zsh-completion` ]]; then
        echo "ERROR: install the cygwin packages: fzf-zsh fzf-zsh-completion"
    else
        source /etc/profile.d/fzf.zsh
        source /etc/profile.d/fzf-completion.zsh
	fi
elif is_os_ubuntu; then
    # NOTE(sdsmith): The ~/.fzf.zsh is used for installing manually. The
    # /usr/share sources are for integration with the ubuntu package.
    #[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
    source /usr/share/doc/fzf/examples/key-bindings.zsh
    source /usr/share/doc/fzf/examples/completion.zsh
else
    echo "WARNING: unknown platform/os, unable to setup fzf"
fi

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
