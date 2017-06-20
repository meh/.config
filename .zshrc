# azzoppati
source "${ZDOTDIR:-$HOME}"/.zshenv
[ -s "${ZDOTDIR:-$HOME}/.zoppo/zoppo.zsh" ] && source "${ZDOTDIR:-$HOME}/.zoppo/zoppo.zsh" #-profile

# aliases
alias vi="vim"
alias vis="vim -S .vim.session"
alias vip="vimpager"
alias screenex="screen -a -c ~/.screenex"
alias stream="livestreamer --player-no-close -p mpv"
alias ida="ida -debug"
alias previ='vim -c "set background=light | colorscheme tomorrow | set statusline=%f\ %h%w%m%r\ %=%(%l,%c%V\ %=\ %P%)"'
alias rust-musl-builder='docker run --rm -it -v "$(pwd)":/home/rust/src ekidd/rust-musl-builder'
alias mupdf="mupdf-x11"

function run {
	${=@} &> /dev/null &
	disown
}

function forget {
	unset HISTFILE
}

function youtube {
  mpv -cache 50000 "${(@)argv[1,-2]}" "$(python3 =youtube-dl -g "$argv[-1]")"
}

function porn {
  mpv --mute=yes --loop=inf $@ &> /dev/null < /dev/null &
}

function twitch {
	ffmpeg -thread_queue_size 4096 \
		-f x11grab -s 1920x844 -r 10 -i :0.0 \
		-f lavfi -ac 1 -i anullsrc \
		-vf colormatrix=bt601:bt709 \
		-c:v libx264 -pix_fmt yuv420p -preset slow -tune zerolatency -b:v 400k -x264opts keyint=20:min-keyint=20:scenecut=-1 \
		-c:a aac -b:a 20k -ar 44100 \
		-threads 8 -f flv "rtmp://live.twitch.tv/app/$TWITCH_TOKEN"
}

# utilities
function gui {
	nohup startx &> /dev/null &!
	exit
}

function pdigest {
	rm -f digest.yml digest.xml

	for i in `find . -maxdepth 1 -name "*-*.rbuild"`; do
		packo-build digest "$i"

		while [ $? = 134 ]; do
			packo-build digest "$i"
		done
	done
}

function pdigest_all {
	ROOT=$PWD

	for dir in `find . -type d`; do
		cd "$dir"
		pdigest
		cd $ROOT
	done
}

function codi {
  local syntax="${1:-ruby}"

  vim -c \
    "let g:startify_disable_at_vimenter = 1 |\
    set bt=nofile ls=0 noru nonu nornu |\
    hi ColorColumn ctermbg=NONE |\
    hi VertSplit ctermbg=NONE |\
    hi NonText ctermfg=0 |\
    Codi $syntax"
}

zmodload zsh/pcre
function soundcloud_download {
  setopt LOCAL_OPTIONS BASH_REMATCH

  local LINE
  local -i MEND=0

  while read LINE; do
    if [[ "$LINE" -pcre-match 'title"\s*:\s*"([^"]+)"' ]]; then
      if [[ "${LINE}" -pcre-match '"username"\s*:\s*"([^"]+)".+?"title"\s*:\s*"([^"]+)".+?"streamUrl"\s*:\s*"([^"]+)"' ]]; then
        wget -O "$BASH_REMATCH[2] - $BASH_REMATCH[3].mp3" "$BASH_REMATCH[4]"
      fi
    fi
  done < =(wget -qo/dev/null -O- "$1")
}

function imgurlast {
	imgur `tail -n 1 ~/random/images/screenshots/screenshots.log`
}

function say {
	echo "$@" | recode utf8..lat1 | festival --tts # --language italian 
}

function cpaste () { gpg -o - -a -c $1 | curl -s -F 'sprunge=<-' http://sprunge.us }

function dpaste () { curl -s $1 | gpg -o - -d }

function tabsize {
  sed "s/\t/$(printf "%$1s")/g"
}

function magnet {
	echo "magnet:?xt=urn:btih:$1&tr=udp://tracker.openbittorrent.com:80/announce"
}

function follow {
	: ${1?"USAGE: follow <url>"}
	curl -sIL "${1}" | sed -r '/^Location/{s/^Location:\s+(.+?)\s*$/\1/g;te};/^.*$/d;:e' | tail -1
}

function tor {
	proxy=http://127.0.0.1:8118/

	export http_proxy=$proxy
	export HTTP_PROXY=$proxy

	export https_proxy=$proxy
	export HTTPS_PROXY=$proxy

	export ftp_proxy=$proxy
	export FTP_PROXY=$proxy
}

function untor {
	unset http_proxy
	unset HTTP_PROXY

	unset https_proxy
	unset HTTPS_PROXY

	unset ftp_proxy
	unset FTP_PROXY
}

untor

function red {
	rebol -qw ~/projects/red/red/red.r $@
}

# Run heatseeker in the current working directory, appending the selected path, if
# any, to the current command, followed by a space.
function insert-heatseeker-path-in-command-line() {
    local selected
    # Move cursor down.
    echo -n -e "\e[B"
    # Find the path; abort if the user doesn't select anything.
    selected=$(rg --files | hs | tr "\n" " ") || return
    # Append the selection to the current command buffer.
    eval 'LBUFFER="$LBUFFER$selected "'
    # Move cursor up once.
    echo -n -e "\e[A"
    # Redraw the prompt since Selecta has drawn several new lines of text.
    zle reset-prompt
}
zle -N insert-heatseeker-path-in-command-line

# Bind the key to the newly created widget
bindkey "^[a" "insert-heatseeker-path-in-command-line"

source ~/.zshrc.private

# daemons
[[ -z `pgrep postino` ]] && postino ~/.mutt/state -b ~/mail/* &> /dev/null &!

[[ -z `pgrep -f hardstatus-daemon` ]] && hardstatus-daemon ~/.hardstatusrc &> /dev/null &!

#[[ -z `pgrep herpes` ]] && herpes ~/.herpesrc &> /dev/null &!

#[[ -z `pgrep -f "torchatd -t$"` ]] && torchatd -t &> /dev/null &!

#[[ -z `pgrep -f "torchatd -t -p anon -l :11100$"` ]] && torchatd -t -p anon -l :11100 &> /dev/null &!

#[[ -z `pgrep tortard` ]] && tortard &> /dev/null &!

[[ -z `pgrep LOLastfm` ]] && LOLastfm &> /dev/null &!

return 0
