library(tuneR)

sonify = 
function(x, y, waveform=c('sine', 'square', 'triangle', 'sawtooth'), 
         duration=2, amp_level = 1,
         stereo=TRUE, smp_rate=44100, flim=c(440, 880), na_freq=300, 
         player='mplayer', player_opts=' > /dev/null 2> /dev/null')
{

  # error checking
  flim = sort(flim)
  waveform = match.arg(waveform)

  # total number of samples
  n = duration * smp_rate
  
  # rescale y values to desired frequency range 
  yr = range(y, na.rm=TRUE)
  yy = (y - yr[1]) / diff(yr) * diff(flim) + flim[1]


  # replace NA's by na_freq
  yy[is.na(yy)] <- na_freq
  
  # interpolate to length n
  yy = spline(x=x, y=yy, n=n)$y 

  # fourier coefficients for waveform
  a = switch(waveform,
    sine = c(1, rep(0, 8)),
    square = c(1, 0, 1/3, 0, 1/5, 0, 1/7, 0, 1/9),
    triangle = c(1, 0, -1/9, 0, 1/25, 0, -1/49, 0, 1/81),
    sawtooth = 1/(1:9)
  )
  
  # waveform with instantaneous frequency yy
  signal = rowSums(
    sapply(seq_along(a), function(i) {
      a[i] * sin(i * 2 * pi * cumsum(yy) / smp_rate)
    })
  )
  
  # multiply amplitude and linear function to simulate movement from left to
  # right
  if (stereo) {
    ramp = seq(0, 1, length=n)
  } else {
    ramp = 0.5
  }
  Rchannel = round(32000 * signal * ramp)
  Lchannel = round(32000 * signal * (1 - ramp))
  
  # construct tuneR wave object
  final = WaveMC(data = data.frame(FR=Rchannel, FL=Lchannel), samp.rate=smp_rate, bit=16)
  final = normalize(final, unit='16', level=amp_level)
  
  # synthesize
  play(final, player, player_opts)

}

# the data to be sonified (standard normal distribution)
x = seq(-3, 3, .01)
y = dnorm(x)
sonify(x, y)

