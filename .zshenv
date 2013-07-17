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
  ~/bin
  /usr/lib/cw
  /usr/bin/vendor_perl
  /usr/bin/core_perl
  ~/.gem/ruby/2.0.0/bin
  ~/.gem/ruby/1.9.1/bin
  ~/.gem/ruby/svn/bin
  ~/.gem/jruby/1.9/bin
  ~/.gem/rbx/1.8/bin
  ~/.gem/rbx/1.9/bin
  ~/projects/external/emscripten
  ~/.vim/bin
  ~/.cabal/bin
  /usr/local/{bin,sbin}
  /usr/{bin,sbin}
  /{bin,sbin}
  /opt/android-sdk/tools
  /opt/android-sdk/platform-tools
  /opt/vmware/bin
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

export MBOX_DAEMON_DIR="${HOME}/mail"
export MBOX_DAEMON_BOXES="inbox; github; twitter; tumblr; mozilla; rss; rss-nsfw; rss-webcomic; rss-kernel; danger; bitlbee; d-bugzilla; travis"
export MBOX_DAEMON_EVERY=120

export INTEL_BATCH=1
export OOO_FORCE_DESKTOP=gnome
export WINEDEBUG=-all

export GTK_IM_MODULE='ibus'
export QT_IM_MODULE='ibus'
export XMODIFIERS=@im='ibus'

# Android
export ANDROID_HOME=/opt/android-sdk
export ANDROID_NDK_HOME=/opt/android-ndk

# Ruby
export RBXOPT="-Xrbc.db=$HOME/.rbc.db -X19"
export JRUBY_OPTS="--1.9"
export RI="-d /usr/share/ri/2.0.0/system"

umask 077
# }}}

# vim: ft=zsh sts=2 ts=2 sw=2 et fdm=marker fmr={{{,}}}
