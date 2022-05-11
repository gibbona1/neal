library(seewave)
#install.packages('soundecology')
library(soundecology)
folder    <- 'richfield_birds_split'
tmp_files <- Sys.glob(file.path(getwd(),folder,'*','*'))
tmp_files <- tmp_files[str_ends(tmp_files,'wav')]
tmp_file  <- tmp_files[1]
tmp_audio <- tuneR::readWave(tmp_file)
seewave::ACI(tmp_audio)
seewave::H(tmp_audio)
seewave::M(tmp_audio)
seewave::NDSI(seewave::soundscapespec(tmp_audio, plot=FALSE))
seewave::Q(seewave::spec(tmp_audio, plot=FALSE, dB="max0", at = 0.5), plot = FALSE)
soundecology::acoustic_diversity(tmp_audio)
soundecology::acoustic_evenness(tmp_audio)
soundecology::bioacoustic_index(tmp_audio)
tmp_folder <- getwd()
setwd('C:/Users/Anthony/Documents/Richfield1/')
seewave::AR(getwd(), datatype="files")


list.files('richfield_birds_split')
