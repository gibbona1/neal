library(seewave)
#install.packages('soundecology')
library(soundecology)
library(stringr)
getwd()
setwd('C:/Users/Anthony/Documents/GitHub/Personal_Projects/bird_detection_audio')

folder    <- 'richfield_birds_split'
tmp_files <- Sys.glob(file.path(getwd(),folder,'*','*'))
tmp_files <- tmp_files[str_ends(tmp_files,'wav')]

df <- data.frame()
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

nrow(df)

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
#setwd('C:/Users/Anthony/Documents/Richfield1/')
#seewave::AR(file.path(getwd(), 'richfield_birds_split'), datatype="files")
#list.files('richfield_birds_split')
