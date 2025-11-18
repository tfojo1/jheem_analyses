## ========= Base R helpers =========

# Sum incidence for 2022–2030 per simulation (last dim = "sim")
sum_inc <- function(arr, yr1 = 2022, yr2 = 2030){
    dn   <- dimnames(arr); nms <- names(dn)
    ykey <- if ("year" %in% nms) "year" else if ("year (complete)" %in% nms) "year (complete)" else stop("No 'year' dim.")
    skey <- if ("sim"  %in% nms) "sim"  else if ("sim (complete)" %in% nms) "sim (complete)" else stop("No 'sim' dim.")
    
    ylab_chr <- trimws(as.character(dn[[ykey]]))
    yrs_num  <- suppressWarnings(as.numeric(ylab_chr))
    if (any(!is.na(yrs_num))) {
        yi <- which(yrs_num >= yr1 & yrs_num <= yr2)
    } else {
        wanted <- as.character(seq.int(yr1, yr2))
        yi <- which(ylab_chr %in% wanted)
    }
    if (!length(yi)) stop("Years ", yr1, "–", yr2, " not found on '", ykey, "'.")
    
    # Index list: TRUE for all dims, replace year dim with yi
    idx <- rep(list(TRUE), length(nms))
    idx[[which(nms == ykey)]] <- yi
    a <- do.call(`[`, c(list(arr), idx, list(drop = FALSE)))
    
    sim_margin <- which(names(dimnames(a)) == skey)
    apply(a, sim_margin, sum, na.rm = TRUE)
}

# Quantile -> formatted string; pct=TRUE for percentages
qfmt <- function(x, pct = FALSE){
    x <- x[is.finite(x) & !is.na(x)]
    if (!length(x)) return("—")
    qs <- as.numeric(stats::quantile(x, c(0.5, 0.025, 0.975), names = FALSE, type = 7))
    if (pct) {
        paste0(sprintf("%.1f", 100*qs[1]), "% [",
               sprintf("%.1f", 100*qs[2]), "–",
               sprintf("%.1f", 100*qs[3]), "%]")
    } else {
        paste0(format(round(qs[1]), big.mark=","), " [",
               format(round(qs[2]), big.mark=","), "–",
               format(round(qs[3]), big.mark=","), "]")
    }
}

# One city row: baseline vs 10, 10(slow), 25, 25(slow)
build_city_row <- function(city, base_obj, d10, d10slow, d25, d25slow){
    b <- sum_inc(base_obj$incidence)
    
    calc <- function(obj){
        v <- sum_inc(obj$incidence) # change back to incidence
        n <- b - v
        p <- ifelse(b > 0, n / b, ifelse(n == 0, 0, NA_real_))
        list(n = n, p = p)
    }
    
    a10     <- calc(d10)
    a10slow <- calc(d10slow)
    a25     <- calc(d25)
    a25slow <- calc(d25slow)
    
    data.frame(
        City = city,
        check.names = FALSE,
        `No doxy: Incident infections (2022–2030)` = qfmt(b),
        `10%: Averted (n)`        = qfmt(a10slow$n),
        `10%: Averted (%)`        = qfmt(a10slow$p, TRUE),
        `25%: Averted (n)`        = qfmt(a25$n),
        `25%: Averted (%)`        = qfmt(a25$p, TRUE)
        
    )
}

## ========= Build table =========
table_inc_averted <- do.call(rbind, list(
    build_city_row("New York, NY",
                   no.intervention.NYC,  sim_doxy10.NYC,  sim_doxy10.NYC.slow,  sim_doxy25.NYC,  sim_doxy25.NYC.slow),
    build_city_row("Miami–Fort Lauderdale, FL",
                   no.intervention.MIA,  sim_doxy10.MIA,  sim_doxy10.MIA.slow,  sim_doxy25.MIA,  sim_doxy25.MIA.slow),
    build_city_row("Atlanta, GA",
                   no.intervention.ATL,  sim_doxy10.ATL,  sim_doxy10.ATL.slow,  sim_doxy25.ATL,  sim_doxy25.ATL.slow),
    build_city_row("Baltimore, MD",
                   no.intervention.BLT,  sim_doxy10.BLT,  sim_doxy10.BLT.slow,  sim_doxy25.BLT,  sim_doxy25.BLT.slow)
    
))

View(table_inc_averted)
kable(table_inc_averted)

# Per-simulation baseline totals by city
b_NYC <- sum_inc(no.intervention.NYC$incidence)
b_MIA <- sum_inc(no.intervention.MIA$incidence)
b_ATL <- sum_inc(no.intervention.ATL$incidence)
b_BLT <- sum_inc(no.intervention.BLT$incidence)

# Combined per-simulation baseline (all four cities)
b_all <- b_NYC + b_MIA + b_ATL + b_BLT

# Pretty quantile string (median [2.5%–97.5%])
qfmt(b_all)


# ===== Combined (all four cities) row =====
build_total_row <- function(label, bases, d10s, d10slows, d25s, d25slows){
    # Sum per-simulation baseline across cities
    b_all <- Reduce(`+`, lapply(bases, \(x) sum_inc(x$incidence)))
    
    # Helper to compute averted counts/percents for a scenario (summed across cities)
    calc_all <- function(sims){
        v_all <- Reduce(`+`, lapply(sims, \(x) sum_inc(x$incidence)))
        n <- b_all - v_all
        p <- ifelse(b_all > 0, n / b_all, ifelse(n == 0, 0, NA_real_))
        list(n = n, p = p)
    }
    
    a10      <- calc_all(d10s)
    a10_slow <- calc_all(d10slows)
    a25      <- calc_all(d25s)
    a25_slow <- calc_all(d25slows)
    
    data.frame(
        City = label,
        check.names = FALSE,
        `No doxy: Incident infections (2022–2030)` = qfmt(b_all),
        `10%: Averted (n)`        = qfmt(a10$n),
        `10%: Averted (%)`        = qfmt(a10$p, TRUE),
        `10% (slow): Averted (n)` = qfmt(a10_slow$n),
        `10% (slow): Averted (%)` = qfmt(a10_slow$p, TRUE),
        `25%: Averted (n)`        = qfmt(a25$n),
        `25%: Averted (%)`        = qfmt(a25$p, TRUE),
        `25% (slow): Averted (n)` = qfmt(a25_slow$n),
        `25% (slow): Averted (%)` = qfmt(a25_slow$p, TRUE)
    )
}

total_row <- build_total_row(
    "(All four cities)",
    bases     = list(no.intervention.NYC, no.intervention.MIA, no.intervention.ATL, no.intervention.BLT),
    d10s      = list(sim_doxy10.NYC,     sim_doxy10.MIA,     sim_doxy10.ATL,     sim_doxy10.BLT),
    d10slows  = list(sim_doxy10.NYC.slow, sim_doxy10.MIA.slow, sim_doxy10.ATL.slow, sim_doxy10.BLT.slow),
    d25s      = list(sim_doxy25.NYC,     sim_doxy25.MIA,     sim_doxy25.ATL,     sim_doxy25.BLT),
    d25slows  = list(sim_doxy25.NYC.slow, sim_doxy25.MIA.slow, sim_doxy25.ATL.slow, sim_doxy25.BLT.slow)
)

table_inc_averted_all <- rbind(table_inc_averted, total_row)
View(table_inc_averted_all)

