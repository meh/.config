# azzoppati
[ -s "${ZDOTDIR:-$HOME}/.zoppo/zoppo.zsh" ] && source "${ZDOTDIR:-$HOME}/.zoppo/zoppo.zsh" # -profile

# aliases
alias vi="vim"
alias vis="vim -S .vim.session"
alias vip="vimpager"
alias screenex="screen -a -c ~/.screenex"

function forget {
	unset HISTFILE
}

function youtube {
  mplayer -cache 50000 "${(@)argv[1,-2]}" "$(python3 =youtube-dl -g "$argv[-1]")"
}

function porn {
  mplayer -ao null -loop 0 "$1" &> /dev/null < /dev/null &
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
	echo "magnet:?xt=urn:btih:$1"
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

source ~/.zshrc.private

# daemons
[[ -z `pgrep mbox-daemon` ]] && mbox-daemon &> /dev/null &!

[[ -z `pgrep hardstatus-daemon` ]] && hardstatus-daemon ~/.hardstatusrc &> /dev/null &!

[[ -z `pgrep herpes` ]] && herpes ~/.herpesrc &> /dev/null &!

[[ -z `pgrep -f "torchatd -t$"` ]] && torchatd -t &> /dev/null &!

[[ -z `pgrep -f "torchatd -t -p anon -l :11100$"` ]] && torchatd -t -p anon -l :11100 &> /dev/null &!

[[ -z `pgrep tortard` ]] && tortard &> /dev/null &!

[[ -z `pgrep LOLastfm` ]] && LOLastfm &> /dev/null &!

[[ -z `pgrep -f skyped` ]] && skyped &> /dev/null &!

return 0
