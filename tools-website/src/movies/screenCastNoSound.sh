ffmpeg -f x11grab -r 30 -s 1024x768 -i :0.0 -aspect 4:3 -vcodec libx264 -vpre lossless_ultrafast -threads 0 $1.mkv
