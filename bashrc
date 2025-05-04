#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias vim='nvim'

alias yt-vid='yt-dlp -f "bestvideo[height<=720]+bestaudio/best[height<=720]"'
alias yt-aud='yt-dlp -x --audio-format mp3'

alias instagd='gallery-dl --cookies ~/.config/gallery-dl/instacookie.txt'

alias wget_website='wget --mirror --convert-links --adjust-extension --page-requisites --no-parent'
alias serve='python3 -m http.server'

joinVid() {
    mkdir -p temp_ts
    mkdir -p joined_videos
    rm -f temp_ts/*.ts

    local i=0
    for f in *.*; do
        if ffprobe -v error "$f" > /dev/null 2>&1; then
            echo "Converting: $f"
            ffmpeg -y -i "$f" -c:v libx264 -c:a aac -preset veryfast -f mpegts "temp_ts/part_$i.ts"
            ((i++))
        fi
    done

    if ls temp_ts/part_*.ts 1> /dev/null 2>&1; then
        echo "Joining all converted .ts files..."
        ffmpeg -y -i "concat:$(ls temp_ts/part_*.ts | tr '\n' '|' | sed 's/|$//')" -c copy "joined_videos/output.mp4"
        echo "✅ Done: All videos joined into joined_videos/output.mp4"
    else
        echo "❌ No valid video files found."
    fi

    rm -rf temp_ts
}

PS1='[\u@\h \W]\$ '
