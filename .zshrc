# prompts
if [[ $TERM == "dumb" ]]; then	# in emacs
    PS1='$ '
    #PS1='%(?..[%?])%!:%~%# '
    # for tramp to not hang, need the following. cf:
    # http://www.emacswiki.org/emacs/TrampMode
    unsetopt zle
    unsetopt prompt_cr
    unsetopt prompt_subst
    unfunction precmd
    unfunction preexec
else

# Keep a log of all history in ~/.logs
    function precmd() {
        if [ "$(id -u)" -ne 0 ]; then
            FULL_CMD_LOG="$HOME/.logs/zsh-history-$(date -u "+%Y-%m-%d").log"
            echo "$USER@`hostname`:`pwd` [$(date -u)] `\history -1`" >> ${FULL_CMD_LOG}
        fi
    }

# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="justin"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

alias grep='grep --color=auto'
alias ll='ls -l'
alias l='ls -al'
alias l.='ls -d .[^.]*'
alias cd..='cd ..'
alias ..='cd ..'
alias gs='git status'
alias df="df -hT"
alias goodnight='sudo pm-suspend'


# Functions
mdc() { mkdir -p "$1" && cd "$1" } # from _why's dotfile http://dotfiles.org/~_why/.zshrc

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
export TERM=xterm-256color

# PATH=$PATH:$HOME/.rvm/bin       # Add RVM to PATH for scripting

# export PATH=/home/justin/.rvm/gems/ruby-1.9.3-preview1@default/bin:/home/justin/.rvm/gems/ruby-1.9.3-preview1@global/bin:/home/justin/.rvm/rubies/ruby-1.9.3-preview1/bin:/home/justin/.rvm/bin:/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin:/usr/bin/core_perl

export PATH=/home/justin/.rvm/bin:/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin:/usr/bin/core_perl
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

fi
