# Sonic Plot

Sonification of data.


# index.html 

Sonification in javascript. Plot data on a map with plotly, and sonify it by hover and click interactions, using audiosynth and speak.js. Run with

    firefox index.html

Text-to-speech is currently not working in chrome.

# Rsonify

Sonification of data in R.
Uses R package tuneR to turn data into sound, and Linux MPlayer for playback.

    source('Rsonify/sonify.R')
    sonify(runif(100))

