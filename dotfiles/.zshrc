# Default Configuration
HISTSIZE=50
SAVEHIST=500
HISTFILE=~/.zsh_history

# Pure Prompt
autoload -U promptinit; promptinit
prompt pure

# Tilix
if [ $TILIX_ID ] || [ $VTE_VERSION ]; then
  source /etc/profile.d/vte.sh
fi
