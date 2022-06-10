folder <- 'misc_data'
aud_df <- data.frame()
for(labfile in list.files(folder)){
  fname <- file.path(folder, labfile)
  finfo <- file.info(fname)
  tmp_table <- read.table(fname, sep = "\t")
  tmp_table
  tmp_df <- data.frame()
  if(tmp_table[2,1] == "\\")
    for(i in 1:(nrow(tmp_table)/2)){
      row_df <- data.frame(date_time   = finfo$ctime,
                           file_name   = paste0(labfile, '.wav'),
                           start_time  = tmp_table[2*i-1,1],
                           end_time    = tmp_table[2*i-1,2],
                           start_freq  = tmp_table[2*i,2],
                           end_freq    = tmp_table[2*i,3],
                           class_label = tmp_table[2*i-1,3],
                           call_type   = "",
                           confidence  = 1,
                           notes       = "",
                           labeler     = "",
                           id          = 1)
      tmp_df <- rbind(tmp_df, row_df)
    }
  else
    for(i in 1:nrow(tmp_table)){
      row_df <- data.frame(date_time   = finfo$ctime,
                           file_name   = paste0(labfile, '.wav'),
                           start_time  = tmp_table[i,1],
                           end_time    = tmp_table[i,2],
                           start_freq  = 0,
                           end_freq    = Inf,
                           class_label = tmp_table[i,3],
                           call_type   = "",
                           confidence  = 1,
                           notes       = "",
                           labeler     = "",
                           id          = 1)
      tmp_df <- rbind(tmp_df, row_df)
    }
  aud_df <- rbind(aud_df, tmp_df)
}
aud_df
