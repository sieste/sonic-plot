#' Sonify data
#'
#' Synthesise a sound with frequency modulated by input data
#'
#' @param y The data values used to modulate the frequency.
#' @param x The x values. Can be used when y values are unevenly spaced. Default is -length(y)/2:length(y)/2
#' @param The waveform used for the sound. One of 'sine', 'square', 'triangle', 'sawtooth'. Default is 'sine'.
#' @param pulse_len Length of individual pulses in seconds. Default is 0.1.
#' @param pulse_amp Amplitude of pulses between 0 and 1. Default is 0.2.
#' @param duration Total duration in seconds. Default is 5.
#' @param amp_level Amplitude level between 0 and 1 to adjust the volume. Default is 1.
#' @param stereo If TRUE a left-to-right transition is simulated. Default is TRUE.
#' @param smp_rate The sampling rate of the wav file. Default is 44100 (CD quality)
#' @param flim The frequency range in Hz to which the data is mapped. Default is c(440, 880).
#' @param na_freq Frequency in Hz that is used for NA data. Default is 300.
#' @param play If TRUE, the sound is played. Default is TRUE. 
#' @param player The program used to play the synthesized wave file. Default is 'mplayer' which is available under Linux. Under windows, try player='mplay32.exe' or player='wmplayer.exe'.
#'
#' @return The synthesized sound saved as a tuneR::WaveMC object.
#' 
#' @example
#' x = seq(-3, 3, .01)
#' y = dnorm(x)
#' sonify(y)
#'
#' @seealso tuneR::play, tuneR::WaveMC
#'
#' @section Licence:
#' GPL (>=2)
#'
#' @author Stefan Siegert \email{stefan_siegert@@gmx.de}
#'
#' @export 


library(tuneR)

sonify = 
function(y, x=seq_along(y)-round(length(y)/2),
         waveform=c('sine', 'square', 'triangle', 'sawtooth'), 
         ticks = 0, tick_len = 0.05, 
         pulse_len=0.1, pulse_amp=0.2,
         duration=5, amp_level = 1, IndicateNegatives=0.5,
         stereo=TRUE, smp_rate=44100, flim=c(440, 880), na_freq=300, play=TRUE)
{

  # error checking
  flim = sort(flim)
  ticks = sort(ticks)
  waveform = match.arg(waveform)

  # auxiliary quantities
  n = duration * smp_rate
  y_ran = range(y, na.rm=TRUE)
  x_ran = range(x, na.rm=TRUE)

  # function to make signal
  MakeSignal <- function(yy, waveform=waveform, ...) {
    # fourier coefficients for different waveform
    a = switch(waveform,
      sine = 1,
      square = c(1, 0, 1/3, 0, 1/5, 0, 1/7, 0, 1/9),
      triangle = c(1, 0, -1/9, 0, 1/25, 0, -1/49, 0, 1/81),
      sawtooth = 1/(1:9)
    )
    # create waveform with instantaneous frequency yy
    sig = rowSums(
    sapply(seq_along(a), function(i) {
        a[i] * sin(i * 2 * pi * cumsum(yy) / smp_rate)
      })
    )
    return(sig)
  }
  
  
  # rescale y values to desired frequency range 
  yy = (y - y_ran[1]) / diff(y_ran) * diff(flim) + flim[1]

  # replace NA's by na_freq
  yy[is.na(yy)] = na_freq
  
  # interpolate to length n
  interp = spline(x=x, y=yy, n=n)
  xx = interp$x
  yy = interp$y
  
  # make signal for range of x and yy
  signal = MakeSignal(yy, waveform=waveform)

  # indicate ticks by sawtooth burst
  n_tick_half = round(tick_len * smp_rate / 2)    
  for (i in seq_along(ticks)) {
    tick_ = ticks[i]
    if(tick_ > x_ran[1] & tick_ < x_ran[2]) {
      # ind is largest index smaller than tick index
      ind = which.max(xx[xx < tick_])
      xinds = (ind - n_tick_half):(ind + 1 + n_tick_half)
      xinds = xinds[xinds > 0 & xinds <= n]
      signal[xinds] = MakeSignal(yy[xinds], waveform='sawtooth')
    }
  }
    
  # add pulses
  if (pulse_len > 0) {
    n_pulse_half = round(pulse_len * smp_rate / 2)
    i_pulses = round((x - min(x)) / diff(range(x)) * (n-1)) + 1
    for (i in seq(-n_pulse_half, n_pulse_half)) {
      j = i_pulses + i
      j = j[j > 0 & j <= n]
      signal[j] = signal[j] + pulse_amp * runif(1)
    }
  }

  # add white noise to negative values of signal if applicable
  if(IndicateNegatives > 0 & y_ran[1] < 0) {
    # find negative values of interpolated y
    transf_zero = flim[1] + (-y_ran[1] / diff(y_ran)) * diff(flim)
    Negatives = which(yy < transf_zero)
    signal[Negatives] <- signal[Negatives] + IndicateNegatives * runif(length(Negatives))
  }
    
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
  if (play) {
    if (Sys.info()[['sysname']] == 'Linux') {
      play(final, 'mplayer', ' > /dev/null 2> /dev/null')
    } else {
      play(final)
    }
  }

  # return the synthesized WaveMC object
  invisible(final)

}
