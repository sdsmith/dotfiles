# Setup fzf
# ---------
if ! command -v fzf &>/dev/null; then
    echo "WARNING: no 'fzf' command. Please install fzf via apt-get"
    exit 1
fi

# NOTE(stewarts): dist copy does this.
# if [[ ! "$PATH" == *$fzf_base_esc/bin* ]]; then
#     export PATH="${PATH:+${PATH}:}$fzf_base/bin"
# fi

# Auto-completion
# ---------------
# TODO(stewarts): They don't provide completions for zsh in Ubuntu packgage...

# Key bindings
# ------------
source /usr/share/doc/fzf/examples/key-bindings.zsh    
