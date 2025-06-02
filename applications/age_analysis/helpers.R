

get_stats <- function(arr, round=T, digits=0, is.percentage=F) {
    arr_data <- apply(arr, 'year', function(one_year) {
        c(quantile(one_year, probs=0.025), median(one_year), upper = quantile(one_year, probs=0.975))
    })
    if (round) arr_data <- round(arr_data, digits=digits)
    if (is.percentage) arr_data <- arr_data * 100
    array(
        arr_data,
        dim = sapply(stats_dimnames, length),
        dimnames = stats_dimnames)
}

#' @description
#' Pads out the age dimension so that 'restratify.age.counts' acts reasonably
#' 
do_prepare_for_restratify <- function(arr) {
    arr_reordered <- apply(arr, c('year', 'sim', 'age'), function(x) {x})
    new_dimnames <- dimnames(arr_reordered)
    new_dimnames$age[5] <- "55-100 years"
    new_dimnames$age <- c(new_dimnames$age, "101-110 years")
    arr_reordered <- c(arr_reordered, rep(0, dim(arr_reordered)["year"] * dim(arr_reordered)["sim"]))
    arr_restored <- array(arr_reordered, sapply(new_dimnames, length), new_dimnames)
    apply(arr_restored, c('year', 'age', 'sim'), function(x) {x})
}

get_med_age <- function(data) {
    apply(do_prepare_for_restratify(data), c('year', 'sim'), function(one_year_sim) {
        restratified <- restratify.age.counts(one_year_sim, desired.age.brackets = 13:100)
        transformed <- cumsum(restratified)/sum(restratified)
        med_age <- names(sort(abs(0.5 - transformed)))[1]
        as.numeric(strsplit(med_age, " years")[[1]][1])
    })
}
get_num_over_55 <- function(data) {
    apply(data, c('year', 'sim'), function(one_year_sim) {
        one_year_sim["55+ years"]
    })
}

get_prop_over_55 <- function(data) {
    apply(data, c('year', 'sim'), function(one_year_sim) {
        one_year_sim["55+ years"] / sum(one_year_sim)
    })
}

format_med_and_interval <- function(arr, is.percentage=F) {
    unlist(lapply(dimnames(arr)[['year']], function(year) {
        year_data <- arr[,year]
        rv <- c(year_data["median"], paste0(year_data["lower"], "-", year_data["upper"]))
        if (is.percentage) rv <- sapply(rv, function(x) {paste0(x, "%")})
        rv[2] <- paste0("[", rv[2], "]")
        as.character(rv)
    }))
}

#' @description
#' Takes every line of the matrix and makes two lines,
#' double-stacking each pair of values in the line.
#' This is so that median is stacked above the CI interval.
#'
convert_to_double_rows <- function(mat) {
    k <- 2 # since horizontal pairs converted to vertical pairs
    m <- nrow(mat)
    n <- ncol(mat) / k

    new_col_length <- k * m
    
    indices_per_col <- rep(c(1, 1 + m), m) + rep(1:m - 1, each=k)
    new_indices <- rep(indices_per_col, n) +
        rep(new_col_length * (1:n - 1), each=new_col_length)
    
    new_mat <- mat[new_indices]
    matrix(new_mat, ncol=n)
}
