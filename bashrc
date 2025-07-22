#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Autostart tmux only if not already inside
[ -z "$TMUX" ] && exec tmux

alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias vim='nvim'
alias yt-vid='yt-dlp -f "bestvideo[height<=720]+bestaudio/best[height<=720]"'
alias yt-aud='yt-dlp -x --audio-format mp3'
alias instagd='gallery-dl --cookies ~/.config/gallery-dl/instacookie.txt'
alias wget_website='wget --mirror --convert-links --adjust-extension --page-requisites --no-parent'
alias serve='python3 -m http.server'
alias record='ffmpeg -video_size 2112x1188 -framerate 30 -f x11grab -i :0.0 output.mp4'

PS1='[\u@\h \W]\$ '
