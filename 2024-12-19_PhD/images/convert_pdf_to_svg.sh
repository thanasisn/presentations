#!/usr/bin/env bash
## created on 2024-09-13

#### ..enter description here..

if [ "${1,,}" = "force" ]; then
  echo "Re do all the conversions"
  FORCE=true
else
  FORCE=false
fi

## convert pdfs to svg
for pdff in *.pdf ; do
  svgf="${pdff%.pdf}.svg"
  echo "$pdff" "$svgf"
  if [[ ! -e "$svgf" ]] || [[ "$pdff" -nt "$svgf" ]] || [[ "$FORCE" == true ]] ; then
    echo "Create file"
    inkscape --export-type="svg" "$pdff"
  fi
done

for pdff in *.pdf ; do
  pngf="${pdff%.pdf}.png"
  echo "$pdff" "$pngf"
  if [[ ! -e "$pngf" ]] || [[ "$pdff" -nt "$pngf" ]] || [[ "$FORCE" == true ]] ; then
    echo "Create file"
    gs -dSAFER -r600 -sDEVICE=pngalpha -o "$pngf" "$pdff"
  fi
done

##  END  ##
exit 0 
