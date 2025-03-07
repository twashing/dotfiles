#!/usr/bin/env sh

ffmpeg -y -f avfoundation -t 5 -i ":0" -c:a aac /Users/timothyw/Audio/recording-20250302-181844.m4a
