# Paths {{{
typeset -gU cdpath fpath mailpath manpath path
typeset -gUT INFOPATH infopath

# set the the list of directories that cd searches
cdpath=(
  $cdpath
)

# set the list of directories that info searches for manuals
infopath=(
  $HOME/.local/share/info
  /usr/local/share/info
  /usr/share/info
  $infopath
)

# set the list of directories that man searches for manuals
manpath=(
  $HOME/.local/share/man
  /usr/local/share/man
  /usr/share/man
  $manpath
)

for path_file ('/etc/manpaths.d/'*(.N))
  manpath+=($(<$path_file))
unset path_file

# set the list of directories that Zsh searches for programs
path=(
  $HOME/bin
  $HOME/.cargo/bin
  $HOME/.local/{bin,sbin}
  $HOME/.gem/ruby/2.4.0/bin
  /opt/android-sdk/tools
  /opt/android-sdk/tools/bin
  /opt/android-sdk/platform-tools
  /opt/android-ndk
  /usr/local/{bin,sbin}
  /usr/{bin,sbin}
  /{bin,sbin}
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

# Editor {{{
if (( $+commands[vim] )); then
  export EDITOR='vim'
  export VISUAL='vim'
elif (( $+commands[emacs] )); then
  export EDITOR='emacs'
  export VISUAL='emacs'
elif (( $+commands[nano] )); then
  export EDITOR='nano'
  export VISUAL='nano'
elif (( $+commands[vi] )); then
  export EDITOR='vi'
  export VISUAL='vi'
fi
# }}}

# Pager {{{
if (( $+commands[vimpager] )); then
  export PAGER='vimpager'
else
  export PAGER='less'

  # mouse-wheel scrolling has been disabled by -X (disable screen clearing)
  # remove -X and -F (exit if the content fits on one screen) to enable it.
  export LESS='-F -g -i -M -R -S -w -X -z-4'

  # set the less input preprocessor
  if (( $+commands[lesspipe.sh] )); then
    export LESSOPEN='| lesspipe.sh %s 2>&-'
  fi
fi
# }}}

# Browser {{{
if [[ "$OSTYPE" == darwin* ]]; then
  export BROWSER='open'
elif (( $+commands[xdg-open] )); then
  export BROWSER='xdg-open'
elif (( $+commands[firefox] )); then
  export BROWSER='firefox'
elif (( $+commands[chromium] )); then
  export BROWSER='chromium'
elif (( $+commands[opera] )); then
  export BROWSER='opera'
fi
# }}}

# Language {{{
if [[ -z "$LANG" ]]; then
  export LANG=en_US.UTF-8
fi

if [[ -z "$LC_ALL" ]]; then
  export LC_ALL=en_US.UTF-8
fi
# }}}

export RUSTFLAGS="-Ctarget-cpu=native"
export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
export CARGO_INCREMENTAL=1

export LD_LIBRARY_PATH=$(rustc --print sysroot)/lib:$LD_LIBRARY_PATH

# Fix Android Studio.
export _JAVA_AWT_WM_NONREPARENTING=1
export ANDROID_HOME=/opt/android-sdk
export ANDROID_NDK_HOME=/opt/android-ndk

# FZF
export FZF_DEFAULT_COMMAND='rg --files --follow'
export FZF_DEFAULT_OPTS='
  --color fg:-1,bg:-1,hl:1,fg+:255,bg+:235,hl+:231
  --color info:240,prompt:255,spinner:208,pointer:196,marker:208
'

# vim: ft=zsh sts=2 ts=2 sw=2 et fdm=marker fmr={{{,}}}
