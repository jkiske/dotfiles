# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
ZSH_THEME="kiske"

# Change the defult custom folder $ZSH/custom?
ZSH_CUSTOM=$HOME/.zsh/custom

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
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

# User configuration
source $ZSH/oh-my-zsh.sh

if [[ "$(uname)" == "Darwin" ]]; then
    # If Mac
    export PATH="/opt/local/sbin:/opt/local/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/local/Library/Frameworks/Python.framework/Versions/2.7/bin:/usr/local/heroku/bin"
    export PYTHONPATH="/opt/local/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/site-packages:$PYTHONPATH"
    export DYLD_FALLBACK_LIBRARY_PATH="/opt/local/lib:/usr/lib/:$DYLD_FALLBACK_LIBRARY_PATH"
elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
    # If Linux
fi

# Aliases
EMACS_OSX='/Applications/Emacs.app/Contents/MacOS/Emacs'
if [[ ! -e $EMACS_OSX ]]; then
    export EDITOR="emacs -nw"
else
    export EDITOR="$EMACS_OSX -nw --no-desktop"
    alias em=$EMACS_OSX
    alias emacs=$EMACS_OSX
fi

if [[ -e "$HOME/.z" ]]; then
    export _Z_CMD="j"
    export _Z_DATA="$HOME/.z/z_data"
    source $HOME/.z/z.sh
fi

if [[ -d /etc/arcanist ]]; then
    source /etc/arcanist/resources/shell/bash-completion
fi
