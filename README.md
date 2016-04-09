# Sonic Plot

Sonification of data.


# mapsonify

Sonification in javascript. Plot data on a map with plotly, and sonify it by hover and click interactions, using p5.sound. Run with

    firefox mapsonify/index.html


# Rsonify

Sonification of data in R.
Uses R package tuneR to turn data into sound. Requires `mplayer` when run in Linux.

    source('Rsonify/sonify.R')
    sonify(runif(100))

