#!/usr/bin/env bash
ffmpeg -i $1 -filter:v scale=640:-1 -c:v libx264 -crf 20 -c:a copy $2
