# Input for the future change 10-year ratio likelihood

# Process: For several 10-year windows, compute the ratio of each city's
# PS diagnoses in the end year versus the start year, only if both numbers
# are at least 50.
# This collection of ratios will later be log-transformed to form a lognormal
# distribution that we'll use to penalize simulations that have a 2030 vs. 2020
# ratio higher than the 95% percentile of this distribution (more than log(10))

# try on all cities
agg_mat <- SURVEILLANCE.MANAGER$data$ps.syphilis.diagnoses$estimate$cdc.aggregated.county$cdc.sti$year__location
agg_mat_all_locs <- dimnames(agg_mat)$location
agg_mat <- agg_mat[,agg_mat_all_locs[get.location.type(agg_mat_all_locs) == "CBSA"]]

ten_year_ratio_start_years <- c(2007:2012)

ten_year_ratio <- unlist(lapply(ten_year_ratio_start_years, function(start_year) {
    apply(agg_mat, "location", function(x) {
        
        start_yr = as.character(start_year)
        end_yr = as.character(start_year + 10)
        
        if (is.na(x[start_yr]) || is.na(x[end_yr])) return(NA)
        
        # Only if start and end have at least 50 cases
        if (x[start_yr] < 50 || x[end_yr] < 50) return(NA)
        
        x[end_yr]/ x[start_yr]
    })}))

ten_year_ratio <- ten_year_ratio[!is.na(ten_year_ratio)]

(mean(log(ten_year_ratio))) # 0.938
(sd(log(ten_year_ratio))) # 0.588
