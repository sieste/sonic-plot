library(tuneR)

# the data to be sonified (standard normal distribution)
x = seq(-3, 3, .01)
y = dnorm(x)

# sampling rate, duration in seconds, number of samples
smp_rate = 44100
total_duration = 3
n = total_duration * smp_rate

# rescale y values to frequency range [440, 880]
yy = (y - min(y)) / diff(range(y)) * 440 + 440

# interpolate to length n
yy = spline(x=x, y=yy, n=n)$y 

# construct sine wave with instantaneous frequency yy
signal = cos(2 * pi * cumsum(yy) / smp_rate)

# multiply amplitude and linear function to simulate movement from left to
# right
amplitude = 32000
ramp = seq(0, 1, length=n)
Rchannel = round(amplitude * signal * ramp)
Lchannel = round(amplitude * signal * (1 - ramp))

# construct tuneR wave object
final = WaveMC(data = data.frame(FR=Rchannel, FL=Lchannel), samp.rate=smp_rate)

# play
play(final, 'mplayer')

