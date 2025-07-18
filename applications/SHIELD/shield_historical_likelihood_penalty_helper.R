# 1. Load historical data
hist_df <- read.table(text = "
year  value
1970  91382
1971  95997
1972  91149
1973  87469
1974  83771
1975  80356
1976  71761
1977  64621
1978  64875
1979  67049
1980  68832
1981  72799
1982  75579
1983  74637
1984  69872
1985  67563
1986  67779
1987  87286
1988 104546
1989 115089
1990 135590
1991 128719
1992 114730
1993 102612
", header = T)

# 2. Compute baseline, max ratio, and sd(log ratio)
baseline_1990 <- hist_df$value[hist_df$year == 1990]
hist_df$ratio <- baseline_1990 / hist_df$value 

hist_df$log_ratio <- log(hist_df$ratio)

max_ratio_hist <- max(hist_df$ratio, na.rm = T)  
min_ratio_hist <- min(hist_df$ratio, na.rm = T) 
sdlog_hist        <- sd(hist_df$log_ratio, na.rm = T) 
