#!/usr/bin/env bash
## created on 2024-11-23

#### Get "all" images from qmd file and create a video

list="$(grep -o "\!\[.*]\(.*\)" "$1" | sed -e 's/!.*(//' -e 's/).*//' | grep "images")"

## create a list of files to make video
echo "$list" | while read line; do
  testf="${line%.*}.png"
  if [ -f $testf ]; then
    echo "file '$testf'" | sed -e 's/images\///'
  fi
done > "./images/.images.list"

cat "./images/.images.list"

cd ./images || exit

ffmpeg -f concat -r 2 -i .images.list -c:v libx264 -profile:v high -crf 20 -vf "pad=ceil(iw/2)*2:ceil(ih/2)*2" -pix_fmt yuv420p fffffpng.mp4

##  END  ##
exit 0 
