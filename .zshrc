## Config oh-my-zsh
# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
ZSH_THEME="kiske"

# Change the defult custom folder $ZSH/custom?
ZSH_CUSTOM=$HOME/.zsh/custom

plugins=(
    common-aliases
    git-extras
    macports
    osx
    pip
    python
    terminalapp
    git
    web-search
)
source $ZSH/oh-my-zsh.sh

### User configuration
## Platform
if [[ "$(uname)" == "Darwin" ]]; then
    export PATH="/opt/local/sbin:/opt/local/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/local/Library/Frameworks/Python.framework/Versions/2.7/bin:/usr/local/heroku/bin"
    export PYTHONPATH="/opt/local/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/site-packages:$PYTHONPATH"
    export DYLD_FALLBACK_LIBRARY_PATH="/opt/local/lib:/usr/lib/:$DYLD_FALLBACK_LIBRARY_PATH"
    # iterm2 integration
    if [[ -e "${HOME}/.iterm2_shell_integration.zsh" ]]; then
        source "${HOME}/.iterm2_shell_integration.zsh"
    fi
elif [[ "$(expr substr $(uname -s) 1 5)" == "Linux" ]]; then
    if [[ -e /opt/ros/jade ]]; then
        source /opt/ros/jade/setup.zsh
    fi
    export PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/etc/arcanist/bin:$PATH"
    export DYLD_FALLBACK_LIBRARY_PATH="/usr/lib/:$DYLD_FALLBACK_LIBRARY_PATH"
    export TERM=xterm-256color
fi

# Aliases
alias donut="sudo mount -t nfs donut1:/donut /donut"
# Package 'common-aliases' makes rm = rm -i which is annoying
unalias rm

# Emacs
EMACS_OSX='/Applications/Emacs.app/Contents/MacOS/Emacs'
if [[ ! -e $EMACS_OSX ]]; then
    export EDITOR="emacs -nw"
else
    export EDITOR="$EMACS_OSX -nw --no-desktop"
    alias em=$EMACS_OSX
    alias emacs=$EMACS_OSX
fi

# z autocomplete
if [[ -e "$HOME/.z" ]]; then
    export _Z_CMD="j"
    export _Z_DATA="$HOME/.z/z_data"
    source $HOME/.z/z.sh
fi

# Arcanist
if [[ -d /etc/arcanist ]]; then
    source /etc/arcanist/resources/shell/bash-completion
fi
