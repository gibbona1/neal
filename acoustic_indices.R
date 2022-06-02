library(seewave)
#install.packages('soundecology')
library(soundecology)
library(stringr)
getwd()
setwd('~/bird_detection_audio')

folder    <- 'richfield_birds_split'
tmp_files <- Sys.glob(file.path(getwd(),folder,'*','*'))
tmp_files <- tmp_files[str_ends(tmp_files,'wav')]

indices_func <- function(tmp_files, df = data.frame()){
  for(tfile in tmp_files){
    tmp_audio <- tuneR::readWave(tfile)
    qval <- tryCatch(seewave::Q(seewave::spec(tmp_audio, plot=FALSE, dB="max0", at = 0.5), plot = FALSE)$Q,
             error = function(e) NA)
    if(length(qval)==0)
      qval <- NA
    s_spec <- seewave::soundscapespec(tmp_audio, plot=FALSE)
    df_row <- data.frame(
      name = tfile,
      aci  = seewave::ACI(tmp_audio),
      h    = seewave::H(tmp_audio),
      m    = seewave::M(tmp_audio),
      ndsi = seewave::NDSI(s_spec, biophony = 2:nrow(s_spec)),
      q    = qval,
      adi  = soundecology::acoustic_diversity(tmp_audio)$adi_left,
      aei  = soundecology::acoustic_evenness(tmp_audio)$aei_left,
      bi   = soundecology::bioacoustic_index(tmp_audio)$left_area
    )
    df <- rbind(df, df_row)
  }
}

#df <- indices_func(tmp_files)

df <- read.csv('richfield_bioacoustic_indices.csv')

nrow(df)
tmp_audio <- tuneR::readWave('richfield_birds_split/Rook/xc683653_start_0_0.wav')
seewave::acoustat(tmp_audio, plot = FALSE)

length(unique(df$name))

length(tmp_files)
df <- df[!duplicated(df$name),]

nrow(df)

df <- read.csv("richfield_bioacoustic_indices.csv")
length(unique(df$name))

unique(sapply(df$name, function(x) paste(str_split(x, '/')[[1]][8:10], collapse = '\\')))

#df_unique$name
#tmp_files
rest_files <- setdiff(tmp_files, df_unique$name)
#tmp_folder <- getwd()
#setwd('~/Richfield1/')

seewave::AR("richfield_birds_split/Rook", datatype="files")
tmp_files
tmp_audio@samp.rate
tmp_t <- timer(tmp_audio, threshold=3, msmooth=c(400,90), dmin=0.05/60)
tmp_t$s
#seewave::AR(file.path(getwd(), 'richfield_birds_split'), datatype="files")
#list.files('richfield_birds_split')

argmax <- function(x, y, w=1, ...) {
  require(zoo)
  n <- length(y)
  y.smooth <- loess(y ~ x, ...)$fitted
  y.max <- rollapply(zoo(y.smooth), 2*w+1, max, align="center")
  delta <- y.max - y.smooth[-c(1:w, n+1-1:w)]
  i.max <- which(delta <= 0) + w
  list(x=x[i.max], i=i.max, y.hat=y.smooth)
}

test <- function(x, y, w, span) {
  peaks <- argmax(x, y, w=w, span=span)
  
  plot(x, y, cex=0.75, col="Gray", main=paste("w = ", w, ", span = ", 
                                              span, ", peaks = ", 
                                              length(peaks$x), sep=""))
  lines(x, peaks$y.hat,  lwd=2) #$
  y.min <- min(y)
  sapply(peaks$i, function(i) lines(c(x[i],x[i]), c(y.min, peaks$y.hat[i]),
                                    col="Red", lty=2))
  points(x[peaks$i], peaks$y.hat[peaks$i], col="Red", pch=19, cex=1.25)
}

ms <- meanspec(tmp_audio)
peaks <- fpeaks(ms)

test(ms[,1], ms[,2], 2, 0.05)
argmax(ms[,1], ms[,2], w=2, span=0.05)

install.packages("GGally")
library(GGally)
ggpairs(df, columns = 3:ncol(df))
colnames(df)
