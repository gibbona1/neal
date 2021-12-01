params <- list(window_size    = 0.50,
               fft_overlap    = 0.75,
               fft_win_length = 128/24000) #0.005333333 approx 5ms
params$window_width <- round(params$window_size / ((1-params$fft_overlap)*params$fft_win_length))
