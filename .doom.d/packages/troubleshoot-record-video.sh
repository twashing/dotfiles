#!/usr/bin/env sh

ffmpeg -y -f avfoundation -framerate 30 -t 5 -i 0 -c:v libx264 -preset fast -pix_fmt yuv420p /Users/timothyw/Videos/recording-20250302-181453.mp4
