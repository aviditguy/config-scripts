#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias vim='nvim'
alias yt-dlp='yt-dlp -f "bestvideo[height<=720]+bestaudio/best[height<=720]"'
alias instaloader='instaloader --login showmanisidlying'
alias wget_website='wget --mirror --convert-links --adjust-extension --page-requisites --no-parent'

PS1='[\u@\h \W]\$ '
