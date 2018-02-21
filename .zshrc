## Config oh-my-zsh
ZDOTDIR="${HOME}/.zsh"

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
ZSH_THEME="kiske"

# Change the defult custom folder $ZSH/custom?
ZSH_CUSTOM=$HOME/.zsh/custom

# bazel autocomplete
if [[ -e "$HOME/.zsh/completion/" ]]; then
    fpath[1,0]=~/.zsh/completion/
    zstyle ':completion:*' use-cache on
    zstyle ':completion:*' cache-path ~/.zsh/cache
fi

# z autocomplete
if [[ -e "$HOME/.z" ]]; then
    export _Z_CMD="j"
    export _Z_DATA="$HOME/.z/z_data"
    source $HOME/.z/z.sh
fi

export TERM=xterm-256color

plugins=(common-aliases git git-extras pip python)

### User configuration
## Platform
if [[ "$(uname)" == "Darwin" ]]; then
    export PATH='/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin'
    EMACS_OSX='/Applications/Emacs/Emacs.app/Contents/MacOS/Emacs'
    if [[ -e $EMACS_OSX ]]; then
        export EDITOR="vim"
        alias em=$EMACS_OSX
        alias emacs=$EMACS_OSX
    fi
    plugins+=(brew osx)
elif [[ "$(expr substr $(uname -s) 1 5)" == "Linux" ]]; then
    export DYLD_FALLBACK_LIBRARY_PATH="/usr/lib/:$DYLD_FALLBACK_LIBRARY_PATH"
    export PATH="$HOME/.opus/bin:/usr/local/cuda/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/anaconda2/bin:$PATH"
    export LD_LIBRARY_PATH="/usr/local/cuda/lib64:/usr/local/cuda/extras/CUPTI/lib64:$LD_LIBRARY_PATH"
    export EDITOR="vim"
fi

source $ZSH/oh-my-zsh.sh

# Package 'common-aliases' makes rm = rm -i which is annoying
unalias rm
# Bazel breaks with this
unalias '...'
