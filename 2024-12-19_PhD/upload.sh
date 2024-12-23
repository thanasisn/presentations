#!/bin/bash
## created on 2023-02-21

#### enter description here

bwlim=500
rclone="$HOME/PROGRAMS/rclone"
config="$HOME/Documents/rclone.conf"
otheropt=" --checkers=20 --delete-before --stats=30s"
bwlimit="  --bwlimit=${bwlim}k"
target="lapauththanasis:/Thesis/presentation"

if [[ "$(hostname)" = "sagan" ]]; then 
    bwlim=500000
    bwlimit="  --bwlimit=${bwlim}k"
    echo "$(hostname)"
fi
# if [[ "$(hostname)" = "sagan" ]]; then 
#     "${rclone}" ${otheropt} ${bwlimit} --config "$config" --include "*.pdf"  sync "$HOME/MANUSCRIPTS/2022_sdr_trends/" "lapauththanasis:/Trends"
# else
#     echo "This runs only on sagan"
# fi

echo "Upload to $target"
"${rclone}" ${otheropt} ${bwlimit} --verbose --config "$config" --max-depth 1 --include "*.{pdf,html,css}" copy "/home/athan/MANUSCRIPTS/presentations/2024-12-19_PhD"                 "$target"
"${rclone}" ${otheropt} ${bwlimit} --verbose --config "$config"                                                 copy "/home/athan/MANUSCRIPTS/presentations/2024-12-19_PhD/Solar_radiation_aerosols_clouds_files" "$target/Solar_radiation_aerosols_clouds_files"
"${rclone}" ${otheropt} ${bwlimit} --verbose --config "$config"               --include "*.{svg}"               copy "/home/athan/MANUSCRIPTS/presentations/2024-12-19_PhD"                 "$target"

echo "BYE"

exit 0 
