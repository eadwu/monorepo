# Default Configuration
export HISTSIZE=50
export SAVEHIST=500
export HISTFILE="${HOME}/.zsh_history"

setopt histignorespace
# Add ~/.local/bin to PATH
export PATH=${PATH}:${HOME}/.local/bin:${HOME}/.cargo/bin:${HOME}/.gem/ruby/2.5.0/bin

# VISUAL
export VISUAL="vim"

# Ruby Gem Installation Path
export GEM_HOME=$(ruby -e 'print Gem.user_dir')

# Aliases
alias docker-clean="docker stop $(docker ps -aq); docker rm $(docker ps -aq); docker rmi $(docker images -q)"

# Pure Prompt
autoload -U promptinit; promptinit
prompt pure
