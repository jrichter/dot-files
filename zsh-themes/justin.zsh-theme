# justin.zsh-theme
# Most of this was taken from
# http://blog.andrewhays.net/love-your-terminal which was taken from
# http://stevelosh.com/blog/2010/02/my-extravagant-zsh-prompt/
# Also some of this was taken from josh.zsh-theme

# Determine what character to use in place of the '$' for my prompt.
function repo_char {
    git branch >/dev/null 2>/dev/null && echo  "%(?,%{%F{green}%},%{%F{red}%})∩%{$reset_color%}" && return
    echo "%(?,%{%F{green}%},%{%F{red}%})⚡%{$reset_color%}"
}

# Display any virtual env stuff with python.
function virtualenv_info {
    [ $VIRTUAL_ENV ] && echo '('`basename $VIRTUAL_ENV`') '
}

# All of my git variables.
ZSH_THEME_GIT_PROMPT_PREFIX=" %{$fg[blue]%}("
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY=")%{$fg[green]%}!"
ZSH_THEME_GIT_PROMPT_UNTRACKED="%{$fg[green]%}?"
ZSH_THEME_GIT_PROMPT_CLEAN=") "

function ruby_prompt_position {
    (( spare_width = ${COLUMNS} ))
    prompt_space=" "

    # Determine if /home/user/ is in the prompt and replace it with
    # the correct size because I use a ~ instead of /home/user/
    if [[ ${HOME} == ${PWD} ]]
    then (( path_size = ${#PWD} - ${#HOME} + 1 ))
    elif [[ ${HOME} ==  ${PWD:0:${#HOME}} ]]
    then (( path_size = ${#PWD} - ${#HOME} + 1 ))
    else
        (( path_size = ${#PWD}  ))
    fi

    ruby_version=$(rvm_prompt_info || rbenv_prompt_info)
    branch=$(current_branch)
    branch_size=${#branch}
    ruby_size=${#ruby_version}
    user_machine_size=${#${(%):-%n@%m-}}

    if [[ ${#branch} -eq 0 ]]
    then (( ruby_size = ruby_size + 1 ))
    else
        (( branch_size = branch_size + 5 ))
        if [[ -n $(git status -s 2> /dev/null) ]]; then
            (( branch_size = branch_size  ))
        fi
    fi

    (( spare_width = ${spare_width} - (${user_machine_size} + ${path_size} + ${branch_size} + ${ruby_size}) ))

    while [ ${#prompt_space} -lt $spare_width ]; do
        prompt_space=" $prompt_space"
    done

    prompt="%{%F{green}%}$prompt_space%{%F{yellow}%}$(rvm_prompt_info || rbenv_prompt_info)%{$reset_color%}"

    echo $prompt
}

setopt prompt_subst



# I like a new line between my result and the next prompt.  Makes it easier to see
PROMPT='
%{$fg[yellow]%}%n@%m%{$reset_color%} %{$fg_bold[green]%}${PWD/#$HOME/~}%{$reset_color%}$(git_prompt_info)$(ruby_prompt_position)
$(virtualenv_info)$(repo_char) '

# Display the date.
RPROMPT='$(date "+%x %l:%M%p %Z")'
