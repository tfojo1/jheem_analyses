

get_stats <- function(arr, keep.dimensions='year', round=T, digits=0, include.mean=T, include.quartiles=F, multiply.by.100=F, floor=F) {
    arr_data <- apply(arr, keep.dimensions, function(x) {
        rv <- c(lower = quantile(x, probs=0.025), median = median(x), upper = quantile(x, probs=0.975))
        if (include.quartiles) rv <- c(rv,
                                       lowermid = quantile(x, probs=0.25),
                                       uppermid = quantile(x, probs=0.75))
        if (include.mean) rv <- c(rv, mean = mean(x))
        rv
    })
    if (floor) arr_data <- floor(arr_data)
    if (round) arr_data <- round(arr_data, digits=digits)
    if (multiply.by.100) arr_data <- arr_data * 100
    metric_dimension = c("lower", "median", "upper")
    if (include.quartiles) metric_dimension <- c(metric_dimension, "lowermid", "uppermid")
    if (include.mean) metric_dimension <- c(metric_dimension, "mean")
    final_dimnames <- c(list(metric=metric_dimension),
                        dimnames(arr)[keep.dimensions])
    array(
        arr_data,
        dim = sapply(final_dimnames, length),
        dimnames = final_dimnames)
}

#' @description
#' Pads out the age dimension so that 'restratify.age.counts' acts reasonably
#' 
do_prepare_for_restratify <- function(arr, top.age=100) {
    original_dimensions <- names(dim(arr))
    reordered_dimensions <- c(original_dimensions[original_dimensions!="age"], "age")
    arr_reordered <- apply(arr, reordered_dimensions, function(x) {x})
    new_dimnames <- dimnames(arr_reordered)
    new_dimnames$age[5] <- paste0("55-", as.character(top.age), " years")
    new_dimnames$age <- c(new_dimnames$age, paste0(as.character(top.age+1), "-", as.character(top.age+10), " years"))
    arr_reordered <- c(arr_reordered, rep(0, dim(arr_reordered)["year"] * dim(arr_reordered)["sim"]))
    arr_restored <- array(arr_reordered, sapply(new_dimnames, length), new_dimnames)
    apply(arr_restored, original_dimensions, function(x) {x})
}

get_restratified_ages <- function(data, top.age=100) {
    rv=apply(do_prepare_for_restratify(data, top.age=top.age), c('year', 'sim'), function(one_year_sim) {
        restratify.age.counts(one_year_sim, desired.age.brackets = 13:top.age)
    })
    new_dimnames <- dimnames(rv)
    names(new_dimnames) <- c('age', 'year', 'sim')
    dim(rv) <- sapply(new_dimnames, length)
    dimnames(rv) <- new_dimnames
    rv
}

get_med_age <- function(data, keep.dimensions='year', top.age=100) {
    apply(do_prepare_for_restratify(data, top.age=top.age), c(keep.dimensions, 'sim'), function(one_year_sim) {
        restratified <- restratify.age.counts(one_year_sim, desired.age.brackets = 13:top.age)
        transformed <- cumsum(restratified)/sum(restratified)
        med_age <- names(sort(abs(0.5 - transformed)))[1]
        as.numeric(strsplit(med_age, " years")[[1]][1])
    })
}

get_65_estimates <- function(data, keep.dimensions="year", top.age=100) {
    apply(do_prepare_for_restratify(data, top.age=top.age), c(keep.dimensions, 'sim'), function(one_year_sim) {
        restratified <- restratify.age.counts(one_year_sim, desired.age.brackets = 13:top.age)
        c("under_65" = sum(restratified[paste0(as.character(13:64), " years")]),
          "65_plus" = sum(restratified[paste0(as.character(65:(top.age-1)), " years")]))
    })
}


get_num_over_55 <- function(data, keep.dimensions="year") {
    apply(data, c(keep.dimensions, 'sim'), function(one_year_sim) {
        one_year_sim["55+ years"]
    })
}

get_prop_over_55 <- function(data, keep.dimensions="year", digits=0) {
    apply(data, c(keep.dimensions, 'sim'), function(one_year_sim) {
        round(100 * one_year_sim["55+ years"] / sum(one_year_sim), digits=digits)
    })
}

get_prop_under_35 <- function(data, keep.dimensions="year", digits=0) {
    apply(data, c(keep.dimensions, 'sim'), function(one_year_sim) {
        round(100 * sum(one_year_sim[c("13-24 years", "25-34 years")]) / sum(one_year_sim), digits=digits)
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

map_sex <- function(arr) {
    non_sex_non_sim_dims <- setdiff(names(dim(arr)), c("sex", "sim")) # assume sex last of middle dims
    het_arr <- array.access(arr, list(sex=c("heterosexual_male", "female")))
    msm_arr <- array.access(arr, list(sex="msm"))
    het_arr <- apply(het_arr, c(non_sex_non_sim_dims, "sim"), sum)
    msm_arr <- apply(msm_arr, c(non_sex_non_sim_dims, "sim", "sex"), function(x) {x})
    new_arr_dimnames <- dimnames(arr)[c(non_sex_non_sim_dims, "sim", "sex")]
    new_arr_dimnames$sex <- c("msm", "non_msm")
    new_arr <- array(c(msm_arr, het_arr),
                     sapply(new_arr_dimnames, length),
                     new_arr_dimnames)
    apply(new_arr, names(dim(arr)), function(x) {x})
}
