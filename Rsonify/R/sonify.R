#' Data sonification
#'
#' Sonification (or audification) is the process of representing data by sounds in the audible range. This package provides the R function `sonify` that transforms univariate data, sampled at regular or irregular intervals, into a continuous sound with time-varying frequency. The ups and downs in frequency represent the ups and downs in the data. Sonify provides a substitute for R's plot function to simplify data analysis for the visually impaired.
#'
#' @param x If `y` is unspecified, `x` is taken as the data to be sonified. If `y` is specified, `x` indicates values along the time axis, which can be useful to sonify unevenly spaced data. 
#' @param y The data to be sonified.
#' @param waveform The waveform used for the sound. One of `sine`, `square`, `triangle`, `sawtooth`. Default is `sine`.
#' @param waveform_custom A vector of Fourier coefficients to define a custom waveform. Overrides the `waveform` argument. Default is NULL. 
#' @param ticks The location of x-axis ticks. The ticks are indicated by short bursts of a sawtooth wave (duration set by `tick_len`). The default is NULL (no ticks).
#' @param tick_len The duration of each tick sound.
#' @param pulse_len Length of white-noise pulses (in seconds) to mark the individual x-values. Default is 0.
#' @param pulse_amp Amplitude of pulses relative to the continuous sound. Between 0 and 1. Default is 0.2.
#' @param pulse_adsr Attack-decay-sustain-release (ADSR) envelope for the pulses, described by a 4-element vector. See details. Default is `c(0.5, 0.05, 0.7, 0.35)`.
#' @param interpolation The interpolation method to connect the y-values before generating the sound. One of `spline`, `linear`, `constant`. `spline` and `linear` generate continous transitions between frequencies, `constant` changes frequencies abruptly. Note: If `interpolation=constant`, y[1] is played from x[1] to x[2], y[2] is played from x[2] to x[3], etc, and the last y-value y[n] is played for the duration x[n] - x[n-1]. Default is `spline`.
#' @param duration Total duration of the generated sound in seconds. Default is 5.
#' @param noise_interval White noise is overlayed whenever y is inside this interval (if noise_amp > 0) or outside this interval (if noise_amp < 0). For example, set to c(-Inf, 0) to indicate data in the negative range. Default is c(0,0) (no noise).
#' @param noise_amp Amplitude (between 0 and 1) of the noise used for noise_interval. Negative values (between 0 and -1) invert noise_interval, i.e. noise is overlaid whenever y falls outside `noise_interval`. Default is 0.5.
#' @param amp_level Amplitude level between 0 and 1 to adjust the volume. Default is 1.
#' @param stereo If TRUE a left-to-right transition is simulated. Default is TRUE.
#' @param smp_rate The sampling rate of the wav file. Default is 44100 (CD quality)
#' @param flim The frequency range in Hz to which the data is mapped. The frequency mapping is linear. Default is c(440, 880).
#' @param na_freq Frequency in Hz that is used for NA data. Default is 300.
#' @param play If TRUE, the sound is played. Default is TRUE. 
#' @param player (Path to) a program capable of playing a wave file from the command line. Under windows, the default is "mplay32.exe" or "wmplayer.exe" (as specified in `?tuneR::play`). Under Linux, the default is "mplayer". Under OS X, the default is "afplay". See `?tuneR::play` for details.
#' @param player_args Further arguments passed to the wav player. Ignored when `player` is unspecified. Under Windows the default is `"/play /close"`. Under Linux the default is `&>/dev/null`. Under OS X the default is "". See `?tuneR::play` for details.
#' @param ... additional arguments
#'
#' @return The synthesized sound is returned as a `tuneR::WaveMC` object.
#' 
#' @examples
#' obj = sonify(dnorm(seq(-3,3,.1)), duration=1, play=FALSE)
#' \dontrun{sonify(dnorm(seq(-3,3,.1)), duration=1)}
#'
#' @details
#' The ADSR envelope `y(x)` for `x` between `0` and `1` is calculated from the 4-vector `pulse_adsr=c(a,d,s,r)` as follows: Linearly increase `y` from `0` to `1` for `x` between `0` and `a`. Linearly decrease `y` from `1` to `s` for `x` between `a` and `a+d`. Leave `y` constant at `s` for `x` between `a+d` and `1-r`. Linearly decrease `y` from `s` to `0` for `x` between `1-r` and `1`.
#'
#' @seealso tuneR::play, tuneR::WaveMC
#'
#' @author Stefan Siegert \email{s.siegert@@exeter.ac.uk} (please report bugs!)
#'
#' @section Licence:
#' GPL (>=2)
#'
#' 
#' @importFrom stats approx pnorm runif spline
#' @importFrom utils tail
#' @importFrom tuneR WaveMC normalize play
#'
#' @rdname sonify
#'
#' @export 


sonify.default = 
function(x, y=NULL,
         waveform=c('sine', 'square', 'triangle', 'sawtooth'), 
         waveform_custom=NULL,
         interpolation=c('spline', 'linear', 'constant'),
         duration=5, flim=c(440, 880), 
         ticks=NULL, tick_len=0.05, 
         pulse_len=0, pulse_amp=0.2, pulse_adsr=c(0.5, 0.05, 0.7, 0.35),
         noise_interval=c(0, 0), noise_amp=0.5,
         amp_level=1, na_freq=300, 
         stereo=TRUE, smp_rate=44100, 
         play=TRUE, player=NULL, player_args=NULL, ...)
{

  # error checking
  ################

  # sonify(rnorm(10)) is interpreted as sonify(x=NULL, y=rnorm(10))
  if (is.null(y)) {
    y = x
    x = NULL
  }
  if(is.null(x)) {
    if (length(y) == 1) {
      x = 0
    } else {
      x = seq_along(y) - length(y) / 2
    }
  } 
  stopifnot(length(x) == length(y))
  stopifnot(is.numeric(flim), length(flim)>1)
  stopifnot(all(pulse_adsr >= 0), all(pulse_adsr <= 1), sum(pulse_adsr[c(1,2,4)])<=1)

  flim = sort(flim[1:2])
  if (!is.null(ticks)) ticks = sort(ticks)
  noise_interval = sort(noise_interval)
  noise_amp = min(max(noise_amp, -1), 1)
  waveform = match.arg(waveform)
  interpolation = match.arg(interpolation)

  # if only one y-value is given, set interpolation = spline; only spline
  # interpolation will not throw an error; also spline will return a constant,
  # which is what would be expected from the other interpolation methods
  if (length(x) < 2) {
    interpolation = 'spline'
  }

  # for constant interpolation, append the last y value and the last
  # x-interval; only then will the last y-value be played
  if (interpolation == 'constant') {
    y = c(y, tail(y,1))
    x = c(x, tail(x,1) + diff(tail(x,2))) 
  }

  ################

  # auxiliary quantities
  n = duration * smp_rate
  x_ran = range(x, na.rm=TRUE)
  y_ran = range(y, na.rm=TRUE)
  if (y_ran[1] == y_ran[2]) {
    y_ran = y_ran + c(-1, 1)
  }


  # rescale y values to desired frequency range 
  yy = (y - y_ran[1]) / diff(y_ran) * diff(flim) + flim[1]

  # replace NA's by na_freq
  yy[is.na(yy)] = na_freq
  
  # interpolate to length n
  interp = switch(interpolation,
    spline = spline(x=x, y=yy, n=n),
    linear = approx(x=x, y=yy, n=n),
    constant = approx(x=x, y=yy, n=n, method='constant')
  )
  xx = interp$x
  yy = interp$y
  
  # make signal for range of x and yy
  if (!is.null(waveform_custom)) {
    a = waveform_custom
  } else {
    a = switch(waveform,
      sine = 1,
      square = c(1, 0, 1/3, 0, 1/5, 0, 1/7, 0, 1/9),
      triangle = c(1, 0, -1/9, 0, 1/25, 0, -1/49, 0, 1/81),
      sawtooth = 1/(1:9)
    )
  }
  a = a / sum(a^2)
  signal = MakeSignal(yy, a=a, smp_rate=smp_rate)

  # indicate x-values by notes
  points = make_notes(x=x, xx=xx, yy=yy, adsr=pulse_adsr, 
                      len=pulse_len, smp_rate=smp_rate)
  signal = (1-pulse_amp) * signal + pulse_amp * points
  
  # indicate axis ticks by pulses of white noise 
  n_tick_half = round(tick_len * smp_rate / 2)    
  for (i in seq_along(ticks)) {
    tick_ = ticks[i]
    if(tick_ > x_ran[1] & tick_ < x_ran[2]) {
      # ind is largest index smaller than tick index
      ind = which.max(xx[xx < tick_])
      xinds = (ind - n_tick_half):(ind + 1 + n_tick_half)
      xinds = xinds[xinds > 0 & xinds <= n]
      signal[xinds] = signal[xinds] + runif(length(xinds))
    }
  }


  # add white noise whenever y is within (or outside) `noise_interval`
  if(length(noise_interval) == 2) {
    # rescale noise_interval to frequency range (use same transformation as for y)
    noise_interval = (noise_interval - y_ran[1]) / diff(y_ran) * diff(flim) + flim[1]
    inds = (yy > noise_interval[1] & yy <= noise_interval[2])
    if (noise_amp < 0) {
      inds = !inds
    }
    signal[inds] = signal[inds] + abs(noise_amp) * runif(sum(inds))
  }
    
  # multiply by linear function to simulate left-to-right transition
  if (stereo) {
    ramp = seq(0, 1, length.out=n)
  } else {
    ramp = rep(0.5, times=n)
  }
  Rchannel = round(32000 * signal * ramp)
  Lchannel = round(32000 * signal * (1 - ramp))
  
  # construct tuneR wave object
  final = tuneR::WaveMC(data = data.frame(FR=Rchannel, FL=Lchannel), samp.rate=smp_rate, bit=16)
  final = tuneR::normalize(final, unit='16', level=amp_level)
  
  # synthesize
  if (play) {
    if (is.null(player)) { # try to find a wav player
      if (Sys.info()[['sysname']] == 'Linux') {
        tuneR::play(final, 'mplayer', '&> /dev/null')
      } else if (Sys.info()[['sysname']] == 'Darwin') {
        tuneR::play(final, 'afplay', '')
      } else {
        tuneR::play(final) # use tuneR defaults
      }
    } else {
      tuneR::play(final, player=player, player_args)
    }
  }

  # return the synthesized WaveMC object
  invisible(final)

}


#' Generic sonify function
#' @rdname sonify
#' @export
sonify = function(x=NULL, y=NULL, ...) {
  UseMethod('sonify')
}


# function to make signal
MakeSignal = function(yy, a, smp_rate) {
  # create waveform with instantaneous frequency yy
  sig = rowSums(
  sapply(seq_along(a), function(i) {
      a[i] * sin(i * 2 * pi * cumsum(yy) / smp_rate)
    })
  )
  # fade in and out to avoid clicking
  n = length(yy)
  n_fade = min(1000, n)
  sig[1:n_fade] = sig[1:n_fade] * pnorm(seq(-3,3,len=n_fade))
  sig[(n-n_fade+1):n] = sig[(n-n_fade+1):n] * (1-pnorm(seq(-3,3,len=n_fade)))

  return(sig)
}


# function to create an Attack-Decay-Sustain-Release envelope
# see https://en.wikipedia.org/wiki/Synthesizer for more information
make_adsr_envelope = function(n, adsr) {
  n_a = round(adsr[1] * n)
  n_d = round(adsr[2] * n)
  n_r = round(adsr[4] * n)
  s = adsr[3]
  n_s = n - (n_a + n_d + n_r)
  
  envel = c(
    seq(0, 1, len=n_a), 
    seq(1, s, len=n_d),
    rep(s, n_s),
    seq(s, 0, len=n_r)
  )
  return(envel)
}


make_notes = function(x, xx, yy, adsr, len, smp_rate) {
  n = length(xx)
  signal = rep(0, n)
  n_tick_half = round(len * smp_rate / 2)    
  x_ran = range(xx)
  for (i in seq_along(x)) {
    tick_ = x[i]
    if(tick_ > x_ran[1] & tick_ < x_ran[2]) {
      # ind is largest index smaller than tick index
      ind = which.max(xx[xx < tick_])
      xinds = (ind - n_tick_half):(ind + 1 + n_tick_half)
      xinds = xinds[xinds > 0 & xinds <= n]
      if (length(xinds) > 0) {
        sig = MakeSignal(yy[xinds], a=1/(1:9), smp_rate=smp_rate)
        envel = make_adsr_envelope(length(sig), adsr)
        signal[xinds] = signal[xinds] + sig * envel
      }
    }
  }
  return(signal)
}


#' Sonification of histogram objects
#' 
#' S3 method to sonify histogram objects
#'
#' @param x An object of class histogram
#' @param ... additional arguments passed to sonify
#' @return The synthesized sound is returned as a `tuneR::WaveMC` object.
#'
#' @export
sonify.histogram = function(x, ...) {
  args = list(...)
  args[['x']] = x$breaks[-length(x$breaks)]
  args[['y']] = x$counts
  args[['interpolation']] = 'constant'
  out = do.call(sonify, args)
  invisible(out)
}




