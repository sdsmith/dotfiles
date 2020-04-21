# aliases
alias hga='hg add'
alias hgc='hg commit'
alias hgca='hg commit --amend'
alias hgb='hg branch'
alias hgba='hg branches'
alias hgbk='hg bookmarks'
alias hgco='hg checkout'
alias hgd='hg diff'
alias hged='hg diffmerge'
alias hgp='hg push'
alias hgs='hg status'
alias hgsl='hg log --limit 20 --template "{node|short} | {date|isodatesec} | {author|user}: {desc|strip|firstline}\n"'
alias hgun='hg resolve --list'
# pull and update
alias hgi='hg incoming'
alias hgl='hg pull -u'
alias hglr='hg pull --rebase'
alias hgo='hg outgoing'

function in_hg() {
  if [ $($DOTSFILES_UTILS/in_hg . 2>/dev/null) ]; then
    echo 1
  fi
}

function hg_get_branch_name() {
  if [ $(in_hg) ]; then
    echo $(basename $(HGRCPATH="" HGPLAIN=1 hg root))
  fi
}

function hg_prompt_short_sha() {
    local SHA
    SHA=$(command hg id 2>/dev/null) && echo "$ZSH_THEME_HG_PROMPT_SHA_BEFORE$SHA$ZSH_THEME_HG_PROMPT_SHA_AFTER"
}

function hg_prompt_long_sha() {
    local SHA
    SHA=$(command hg whereami 2>/dev/null) && echo "$ZSH_THEME_HG_PROMPT_SHA_BEFORE$SHA$ZSH_THEME_HG_PROMPT_SHA_AFTER"
}

function hg_prompt_info {
  if [ $(in_hg) ]; then
    _DISPLAY=$(hg_get_branch_name)
    echo "$ZSH_PROMPT_BASE_COLOR$ZSH_THEME_HG_PROMPT_PREFIX\
$ZSH_THEME_REPO_NAME_COLOR$_DISPLAY$ZSH_PROMPT_BASE_COLOR$ZSH_PROMPT_BASE_COLOR$(hg_dirty)$ZSH_THEME_HG_PROMPT_SUFFIX$ZSH_PROMPT_BASE_COLOR"
    unset _DISPLAY
  fi
}

function hg_dirty_choose {
  if [ $(in_hg) ]; then
    hg status 2> /dev/null | command grep -Eq '^\s*[ACDIM!?L]'
    if [ $pipestatus[-1] -eq 0 ]; then
      # Grep exits with 0 when "One or more lines were selected", return "dirty".
      echo $1
    else
      # Otherwise, no lines were found, or an error occurred. Return clean.
      echo $2
    fi
  fi
}

function hg_dirty {
  hg_dirty_choose $ZSH_THEME_HG_PROMPT_DIRTY $ZSH_THEME_HG_PROMPT_CLEAN
}

function hgic() {
    hg incoming "$@" | grep "changeset" | wc -l
}

function hgoc() {
    hg outgoing "$@" | grep "changeset" | wc -l
}

# NOTE: Order modelled after the oh-my-zsh/lib.git.zsh git_prompt_status order.
function hg_prompt_status() {
    local INDEX STATUS
    INDEX=$(command hg status 2>/dev/null)
    STATUS=""
    if $(echo "$INDEX" | grep '^\? ' -m1 &>/dev/null); then
        STATUS="$ZSH_THEME_HG_PROMPT_UNTRACKED$STATUS"
    fi
    if $(echo "$INDEX" | grep '^A ' -m1 &>/dev/null); then
        STATUS="$ZSH_THEME_HG_PROMPT_ADDED$STATUS"
    fi
    if $(echo "$INDEX" | grep '^M ' -m1 &>/dev/null); then
        STATUS="$ZSH_THEME_HG_PROMPT_MODIFIED$STATUS"
    fi
    if $(echo "$INDEX" | grep '^R ' -m1 &>/dev/null); then
        STATUS="$ZSH_THEME_HG_PROMPT_REMOVED$STATUS"
    fi
    if $(echo "$INDEX" | grep '^! ' -m1 &>/dev/null); then
        STATUS="$ZSH_THEME_HG_PROMPT_MISSING$STATUS"
    fi
    echo $STATUS
}
