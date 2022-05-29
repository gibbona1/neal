tmp_table <- read.table('misc_data/LabelTrack.txt', sep = "\t")
tmp_table
tmp_df <- data.frame()
if(tmp_table[2,1] == "\\")
  for(i in 1:(nrow(tmp_table)/2)){
    row_df <- data.frame(file_name   = "",
                         start_time  = tmp_table[2*i-1,1],
                         end_time    = tmp_table[2*i-1,2],
                         start_freq  = tmp_table[2*i,2],
                         end_freq    = tmp_table[2*i,3],
                         class_label = tmp_table[2*i-1,3])
    tmp_df <- rbind(tmp_df, row_df)
  }
else
  for(i in 1:nrow(tmp_table)){
    row_df <- data.frame(file_name   = "",
                         start_time  = tmp_table[i,1],
                         end_time    = tmp_table[i,2],
                         start_freq  = NA,
                         end_freq    = NA,
                         class_label = tmp_table[i,3])
    tmp_df <- rbind(tmp_df, row_df)
  }
tmp_df
