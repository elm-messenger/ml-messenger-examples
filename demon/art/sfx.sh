#!/bin/sh
# Synthesizes the sound effects into assets/sfx with sox. Run from art/.
set -e
o=../assets/sfx
r="-r 44100 -c 1 -b 16"
sox -n $r $o/step.wav   synth 0.045 sine 520:380 fade 0 0.045 0.035 gain -14
sox -n $r /tmp/p1.wav synth 0.13 sine 150:55 fade 0 0.13 0.1
sox -n $r /tmp/p2.wav synth 0.13 brownnoise fade 0 0.13 0.1 gain -6
sox -m /tmp/p1.wav /tmp/p2.wav $o/push.wav gain -6
sox -n $r $o/bump.wav   synth 0.07 square 95:70 gain -6 lowpass 600 fade 0 0.07 0.05 gain -16
sox -n $r $o/heat.wav   synth 0.35 whitenoise gain -6 highpass 2500 tremolo 38 60 fade 0.01 0.35 0.25 gain -13
sox -n $r /tmp/f1.wav synth 0.4 sine 1760 fade 0 0.4 0.32
sox -n $r /tmp/f2.wav synth 0.4 sine 2637 fade 0 0.4 0.32
sox -m /tmp/f1.wav /tmp/f2.wav $o/freeze.wav tremolo 14 40 gain -15
sox -n $r $o/select.wav synth 0.05 triangle 880 fade 0 0.05 0.04 gain -14
sox -n $r $o/undo.wav   synth 0.16 pinknoise bandpass 900 400 fade 0.12 0.16 0.03 gain -8
# win: a rising arpeggio with a little echo
sox -n $r /tmp/n1.wav synth 0.12 triangle 523.25 fade 0 0.12 0.06
sox -n $r /tmp/n2.wav synth 0.12 triangle 659.25 fade 0 0.12 0.06
sox -n $r /tmp/n3.wav synth 0.12 triangle 783.99 fade 0 0.12 0.06
sox -n $r /tmp/n4.wav synth 0.45 triangle 1046.5 fade 0 0.45 0.35
sox /tmp/n1.wav /tmp/n2.wav /tmp/n3.wav /tmp/n4.wav $o/win.wav echo 0.8 0.7 90 0.35 gain -9
# die: a falling groan into a hiss
sox -n $r /tmp/d1.wav synth 0.55 sawtooth 330:70 gain -6 lowpass 1200 fade 0 0.55 0.2
sox -n $r /tmp/d2.wav synth 0.55 whitenoise gain -6 highpass 3000 fade 0.2 0.55 0.3 gain -10
sox -m /tmp/d1.wav /tmp/d2.wav $o/die.wav gain -8
rm -f /tmp/n?.wav /tmp/d?.wav /tmp/p?.wav /tmp/f?.wav
