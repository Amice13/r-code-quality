# Yellow Rail Vocalization Response- Spectrograms
# Date: 9/26/2022
# User: Tabitha Olsen and Chris Butler
# Data Ownership: Tabitha Olsen and Chris Butler
# Aff: University of Central Oklahoma and Texas A&M University

# Resource: https://hansenjohnson.org/post/spectrograms-in-r/


# Set working directory and library
setwd("C:/Users/cbutler11/Dropbox/Sync - Dropbox/Yellow Rail Vocalization/mp3")
library(tuneR)

# The first thing that I want to do is convert the *.mp3 file to a *.wav file
YERA_mp3 <- readMP3("YERA Contact Call 1.mp3")

# Then I want to save it into the WAV folder
setwd("C:/Users/cbutler11/Dropbox/Sync - Dropbox/Yellow Rail Vocalization/WAV")
writeWave(YERA_mp3,"EDITEDwavContact_Call.wav",extensible=FALSE)

#################
## Audio Setup ##
#################

# Reading in our audio file:
YERA_call<-readWave('EDITEDwavContact_Call.wav')

# Extracting the signal:
snd = YERA_call@left

# Determining the duration:
dur = length(snd)/YERA_call@samp.rate
dur # seconds
#1.44

# Determine sample rate (Hz):
fs = YERA_call@samp.rate
fs
#48000 Hz


#######################
## Plotting Waveform ##
#######################


# Demean to remove DC offset:
snd = snd - mean(snd)


# Plot waveform:
YERAwav<-plot(snd, type = 'l', xlab = 'Samples', ylab = 'Amplitude')

# Change to TIFF folder and save waveform in publication dimensions. 
setwd("C:/Users/cbutler11/Dropbox/Sync - Dropbox/Yellow Rail Vocalization/TIFF")
tiff("YERAwav2.tiff", units="in", width=6, height=6, res=300)
plot(snd, type = 'l', xlab = 'Samples', ylab = 'Amplitude')
dev.off()


############################
##  Plotting Spectrogram  ##
############################
library(signal, warn.conflicts = F, quietly = T) # signal processing functions
library(oce, warn.conflicts = F, quietly = T) # image plotting functions and nice color maps


# -Define spectrogram parameters-

# number of points to use for the fft:
nfft=1024

# window size (in points):
window=256

# overlap (in points):
overlap=128

# create spectrogram:
spec = specgram(x = snd,
                n = nfft,
                Fs = fs,
                window = window,
                overlap = overlap
)

# discard phase information
P = abs(spec$S)

# normalize
P = P/max(P)

# convert to dB
P = 10*log10(P)

# config time axis
t = spec$t


# plot spectrogram
imagep(x = t,
       y = spec$f,
       z = t(P),
       col = oce.colorsViridis,
       ylab = 'Frequency [Hz]',
       xlab = 'Time [s]',
       drawPalette = T,
       decimate = F,
       main=expression(paste("Yellow Rail", italic(' (Coturnicops noveboracensis)')))
       
)


#Saving spectrogram in publication dimensions:
tiff("YERASpec.tiff", units="in", width=6, height=6, res=300)
imagep(x = t,
       y = spec$f,
       z = t(P),
       col = oce.colorsViridis,
       ylab = 'Frequency [Hz]',
       xlab = 'Time [s]',
       drawPalette = T,
       decimate = F,
      
)