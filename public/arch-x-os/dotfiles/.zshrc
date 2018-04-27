# Pseudo Aliases
function docker-build () {
  docker build -t $1 -f $1/Dockerfile .
}

function docker-clean () {
  docker stop $(docker ps -aq)
  docker rm $(docker ps -aq)
  docker rmi $(docker images -q)
}

function docker-push-image () {
  docker tag $1 ${DOCKER_ID_USER}/$1
  docker push ${DOCKER_ID_USER}/$1
}

function download-audio () {
  youtube-dl --extract-audio --audio-format mp3 $1
}

function system-upgrade {
  yaourt -Rcns visual-studio-code-insiders
  yaourt -Syu --aur
  yaourt -S visual-studio-code-insiders
}

# VISUAL
export VISUAL="vim"

# DOCKER_ID_USER
export DOCKER_ID_USER="tianxian"

# Ruby Gem Installation Path
export GEM_HOME=$(ruby -e 'print Gem.user_dir')

# Add /opt/anaconda/bin, ~/.local/bin, ~/.cargo/bin, and ~/.gem/ruby/LATEST_RUBY_VERSION/bin to PATH
export PATH=${PATH}:/opt/anaconda/bin:${HOME}/.local/bin:${HOME}/.cargo/bin:${HOME}/.gem/ruby/$(ruby --version | grep -Po '(?!ruby )[0-9\.]+(?=p)')/bin

# Default Configuration
export HISTSIZE=50
export SAVEHIST=500
export HISTFILE="${HOME}/.zsh_history"

setopt histignorespace

# Pure Prompt
autoload -U promptinit; promptinit
prompt pure
