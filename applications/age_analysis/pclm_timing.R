aa <- med_age_timeline_raw_arr[1:5,,1:10,,drop=F]
ll <- length(aa)/5
bb <- get_med_age(aa, keep.dimensions = c("year", "location"), report.duration = T,
                  method='monoH.FC')
cc <- get_med_age(aa, keep.dimensions = c("year", "location"), report.duration = T,
                  method="pclm")

# Perhaps I can do one thread per year?
# 