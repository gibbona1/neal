labs_df <- read.csv("data-2022-06-13.csv")

if("X" %in% colnames(labs_df))
  labs_df <- subset(labs_df, select = -c(X))

labfolders <- gsub('www/', '', list.dirs('www')[-1])
for(lab in labfolders){
  lab_pre <- sapply(labs_df$labeler, function(x) str_split(x, '@')[[1]][1])
  if(lab == 'tmp')
    lab_df <- labs_df[!lab_pre %in% labfolders,]
  else
    lab_df <- labs_df[lab_pre == lab,]
  fname   <- paste0("labels/tmp_labels_", lab, ".csv")
  if(file.exists(fname))
    save_df <- read.csv(fname)
  else
    save_df <- lab_df
  if(nrow(save_df) == 0)
    save_df <- lab_df
  else
    save_df <- rbind(save_df, lab_df[lab_df$date_time > max(save_df$date_time),])
  write.csv(save_df, fname, row.names = FALSE)
}