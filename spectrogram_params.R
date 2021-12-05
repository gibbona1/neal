params <- list(window_size    = 0.60,
               fft_overlap    = 0.75,
               fft_win_length = 512/24000) #0.02133333 approx 21ms
params$window_width <- round(params$window_size / ((1-params$fft_overlap)*params$fft_win_length))