#!/bin/sh
# Rasterizes the SVG art into assets/img. Run from art/.
set -e
python3 demon.py
out=../assets/img
for n in demon demon_cold demon_hot; do rsvg-convert -w 256 -h 256 $n.svg -o $out/$n.png; done
rsvg-convert -w 512 -h 512 demon.svg -o $out/demon_big.png
for n in flame snow; do rsvg-convert -w 128 -h 128 $n.svg -o $out/$n.png; done
