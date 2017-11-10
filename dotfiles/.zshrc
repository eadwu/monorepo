# Default Configuration
HISTSIZE=50
SAVEHIST=500
HISTFILE=~/.zsh_history

# Add ~/.local/bin to PATH
export PATH=${PATH}:${HOME}/.local/bin

# Pure Prompt
autoload -U promptinit; promptinit
prompt pure
