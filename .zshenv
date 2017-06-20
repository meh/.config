# Paths {{{
typeset -gU cdpath fpath mailpath manpath path
typeset -gUT INFOPATH infopath

# set the the list of directories that cd searches
cdpath=(
  $cdpath
)

# set the list of directories that info searches for manuals
infopath=(
  /usr/local/share/info
  /usr/share/info
  $infopath
)

# set the list of directories that man searches for manuals
manpath=(
  /usr/local/share/man
  /usr/share/man
  $manpath
)

for path_file ('/etc/manpaths.d/'*(.N))
  manpath+=($(<$path_file))
unset path_file

# set the list of directories that Zsh searches for programs
path=(
  ~/.bin
  ~/.local/bin
  ~/.latex/2016/bin/x86_64-linux
  /usr/lib/cw
  /usr/bin/vendor_perl
  /usr/bin/core_perl
  ~/.gem/ruby/2.4/bhn
  ~/.go/bin
  ~/projects/external/emscripten
  ~/.vim/bin
  ~/.cabal/bin
  ~/.cargo/bin
  ~/.multirust/toolchains/nightly/cargo/bin
  ~/.raspi/arm-bcm2708/gcc-linaro-arm-linux-gnueabihf-raspbian-x64/bin
  /usr/local/{bin,sbin}
  /usr/{bin,sbin}
  /{bin,sbin}
  /opt/android-sdk/tools
  /opt/android-sdk/platform-tools
  /opt/android-ndk
  /opt/vmware/bin
  /opt/neo4j/bin
  $path
)

for path_file ('/etc/paths.d/'*(.N))
  path+=($(<$path_file))
unset path_file
# }}}

# Temporary Files {{{
if [[ -d "$TMPDIR" ]]; then
  export TMPPREFIX="${TMPDIR%/}/zsh"

  if [[ ! -d "$TMPPREFIX" ]]; then
    mkdir -p "$TMPPREFIX"
  fi
fi
# }}}

# Editors {{{
export EDITOR='vim'
export VISUAL='vim'
export PAGER='vimpager'
export BROWSER='firefox'
# }}}

# Language {{{
export LANG="en_US.UTF-8"
export LANGUAGE="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"
export LC_CTYPE="en_US.UTF-8"
export LC_NUMERIC="it_IT.UTF-8"
export LC_TIME="en_US.UTF-8"
export LC_COLLATE="en_US.UTF-8"
export LC_MONETARY="it_IT.UTF-8"
export LC_MESSAGES="en_US.UTF-8"
export LC_PAPER="en_US.UTF-8"
export LC_NAME="en_US.UTF-8"
export LC_ADDRESS="it_IT.UTF-8"
export LC_TELEPHONE="it_IT.UTF-8"
export LC_MEASUREMENT="it_IT.UTF-8"
export LC_IDENTIFICATION="en_US.UTF-8"
# }}}

# Less Options {{{
# mouse-wheel scrolling has been disabled by -X (disable screen clearing)
# remove -X and -F (exit if the content fits on one screen) to enable it.
export LESS='-F -g -i -M -R -S -w -X -z-4'

# set the less input preprocessor
if (( $+commands[lesspipe.sh] )); then
  export LESSOPEN='| lesspipe.sh %s 2>&-'
fi
# }}}

# Other Stuff {{{
export NODE_PATH="/usr/lib/node_modules"
export GOPATH="$HOME/.go"
export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
export RUSTFLAGS="-Ctarget-cpu=native"

export INTEL_BATCH=1
export WINEDEBUG=-all

#export GTK_IM_MODULE='ibus'
#export QT_IM_MODULE='ibus'
#export XMODIFIERS=@im='ibus'
#export XDG_CURRENT_DESKTOP=GNOME

# Android
export ANDROID_HOME=/opt/android-sdk
export ANDROID_NDK_HOME=/opt/android-ndk

# Ruby
export RBXOPT="-Xrbc.db=$HOME/.rbc.db -X19"
export JRUBY_OPTS="--1.9"
export RI="-d /usr/share/ri/2.1.0/system"

export RUST_NEW_ERROR_FORMAT=true

export GPG_TTY="$(tty)"
unset TERMINFO

export TEXDIR="$HOME/.latex"
export TEXMFHOME="$HOME/.latex"
export TEXMFVAR="$HOME/.latex/texmf-var"
export TEXMFCONFIG="$HOME/.latex/texmf-config"

umask 077
# }}}

# vim: ft=zsh sts=2 ts=2 sw=2 et fdm=marker fmr={{{,}}}
