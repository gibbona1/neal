params <- list(window_size    = 0.60,
               fft_overlap    = 0.5,
               fft_win_length = 256/24000, #0.01066667 approx 10ms
               window_width   = 128) 
params$window_width <- round(params$window_size / ((1-params$fft_overlap)*params$fft_win_length))
