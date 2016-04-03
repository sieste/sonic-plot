#' Sonify data
#'
#' Synthesise a sound with frequency modulated by input data
#'
#' @param y The data values used to modulate the frequency.
#' @param x The x values. Can be used when y values are unevenly spaced. Default is 1:length(y)
#' @param waveform One of 'sine', 'square', 'triangle', 'sawtooth'. Default is 'sine'.
#' @param duration Duration in seconds. Default is 2.
#' @param amp_level Amplitude level between 0 and 1 to adjust the volume. Default is 1.
#' @param stereo If TRUE a left-to-right transition is simulated. Default is TRUE.
#' @param smp_rate The sampling rate of the wav file. Default is 44100 (CD quality)
#' @param flim The frequency range to which the data is mapped. Default is c(440, 880).
#' @param play If TRUE, the sound is played. Default is TRUE. 
#' @param player The program used to play the synthesized wave file. Default is 'mplayer' which is available under Linux. Under windows, try player='mplay32.exe' or player='wmplayer.exe'.
#' @param player_opts Additional options passed to tuneR::play. Default is ' > /dev/null 2> /dev/null' which suppresses mplayer's output and error messages. Under windows, try player_opts='/play /close'.
#'
#' @return The synthesized sound saved as a tuneR::WaveMC object.
#'
#' @author Stefan Siegert \email{stefan_siegert@@gmx.de}
#'
#' @seealso tuneR::play, tuneR::WaveMC
#' 
#' @example
#' x = seq(-3, 3, .01)
#' y = dnorm(x)
#' sonify(y)
#'
#' @export 


library(tuneR)

sonify = 
function(y, x=1:length(y), waveform=c('sine', 'square', 'triangle', 'sawtooth'), 
         duration=2, amp_level = 1,
         stereo=TRUE, smp_rate=44100, flim=c(440, 880), na_freq=300, play=TRUE,
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

  # fourier coefficients for different waveform
  a = switch(waveform,
    sine = 1,
    square = c(1, 0, 1/3, 0, 1/5, 0, 1/7, 0, 1/9),
    triangle = c(1, 0, -1/9, 0, 1/25, 0, -1/49, 0, 1/81),
    sawtooth = 1/(1:9)
  )
  
  # create waveform with instantaneous frequency yy
  signal = rowSums(
    sapply(seq_along(a), function(i) {
      a[i] * sin(i * 2 * pi * cumsum(yy) / smp_rate)
    })
  )
  
  # multiply by linear function to simulate left-to-right transition
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

  # return the synthesized WaveMC object
  invisible(final)

}

