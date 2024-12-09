#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias vim='nvim'
alias hx='helix'
alias yt-vid='yt-dlp -f "bestvideo[height<=720]+bestaudio/best[height<=720]"'
alias yt-aud='yt-dlp -x --audio-format mp3'
alias wget_website='wget --mirror --convert-links --adjust-extension --page-requisites --no-parent'
alias serve='python3 -m http.server'

PS1='[\u@\h \W]\$ '
