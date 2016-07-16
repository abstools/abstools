ffmpeg -i $1 -acodec libfaac -ab 128k -ac 2 -vcodec libx264 -vpre slow -crf 22 -threads 0 $2.mp4
