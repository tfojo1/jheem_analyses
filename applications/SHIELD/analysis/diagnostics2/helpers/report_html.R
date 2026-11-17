# ****************************************************************************************************
# DIAGNOSTICS2: MIXING REPORT (HTML) ----
# ****************************************************************************************************
# Lays out the report. No statistics are computed here except by calling mixing_stats.R and identifiability.R.
#   1. HTML building blocks     esc(), tbl(), img(), item(), write.annotated.csv()
#   2. What we see (METHODS 6)  see.*(): one function per table or figure, each a few lines of rules
#   3. One city                 city.section(): loads the MCMC, computes, builds the city's HTML, frees memory
#   4. The report               write.mixing.report()
# Fixed wording: report_text.R
# ****************************************************************************************************

## 1. HTML BUILDING BLOCKS ----
esc <- function(x) htmltools::htmlEscape(as.character(x))
# numbers: 4 significant digits; 1 decimal from 1,000 up (so log-likelihoods keep their decimals)
fmt <- function(v) {
    if (!is.numeric(v)) return(esc(v))
    if (is.na(v)) return("")
    if (abs(v) >= 1000) format(round(v, 1), big.mark = ",", nsmall = if (v == round(v)) 0 else 1, trim = TRUE)
    else format(signif(v, 4), trim = TRUE)
}

# data frame -> HTML table. Column headers show COLUMN.NOTES on hover.
#   row.class: optional vector of CSS classes (one per row), e.g. the verdict
#   html.cols: columns already in HTML (not escaped), e.g. links
tbl <- function(df, row.class = NULL, html.cols = character(0)) {
    if (is.null(df) || NROW(df) == 0) return("<p class='none'>None.</p>")
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    head <- paste0(vapply(names(df), function(col) {
        note <- COLUMN.NOTES[[col]]
        title <- if (is.null(note)) "" else paste0(" title='", esc(paste(note[note != "-"], collapse = ". ")), "'")
        paste0("<th", title, ">", esc(col), "</th>")
    }, ""), collapse = "")
    rows <- vapply(seq_len(nrow(df)), function(i) {
        cells <- vapply(names(df), function(col)
            paste0("<td>", if (col %in% html.cols) df[[col]][i] else fmt(df[[col]][i]), "</td>"), "")
        cls <- if (!is.null(row.class)) paste0(" class='", tolower(row.class[i]), "'") else ""
        paste0("<tr", cls, ">", paste(cells, collapse = ""), "</tr>")
    }, "")
    paste0("<div class='scroll'><table><tr>", head, "</tr>", paste(rows, collapse = ""), "</table></div>")
}

# a plot drawn by draw() -> embedded PNG
img <- function(draw, width = 1100, height = 600) {
    f <- tempfile(fileext = ".png")
    png(f, width = width, height = height, res = 110)
    ok <- tryCatch({ draw(); TRUE }, error = function(e) { message("plot failed: ", e$message); FALSE },
                   finally = dev.off())
    if (!ok) return("<p class='none'>Plot could not be drawn.</p>")
    paste0("<img alt='plot' src='data:image/png;base64,", base64enc::base64encode(f), "'/>")
}

# one table or figure: title, "How to read", "What we see", content
item <- function(title, how, see, body)
    paste0("<div class='item'><h4>", esc(title), "</h4>",
           "<p class='how'><b>How to read:</b> ", how, "</p>",
           "<p class='see'><b>What we see:</b> ", see, "</p>", body, "</div>")

# CSV whose first two rows describe each column (WHAT IT IS, HOW TO READ); read back with read.csv(file, skip = 2)
write.annotated.csv <- function(df, file) {
    notes <- lapply(names(df), function(col) if (!is.null(COLUMN.NOTES[[col]])) COLUMN.NOTES[[col]] else c("", ""))
    notes <- as.data.frame(do.call(cbind, notes), stringsAsFactors = FALSE)
    notes[[1]] <- paste0(c("WHAT IT IS: ", "HOW TO READ: "), notes[[1]])
    con <- file(file, open = "w"); on.exit(close(con))
    write.table(notes, con, sep = ",", row.names = FALSE, col.names = FALSE, qmethod = "double")
    write.table(df, con, sep = ",", row.names = FALSE, col.names = TRUE, qmethod = "double")
}


## 2. WHAT WE SEE (METHODS 6) ----
# Each function returns one sentence (HTML-escaped) built only from the table it describes.
n.of <- function(n, word) paste0(n, " ", word, if (n == 1) "" else "s")
verb <- function(n, singular, plural) if (n == 1) singular else plural

see.overview <- function(ov) {
    counts <- table(factor(ov$verdict, levels = c("FAIL", "CHECK", "PASS")))
    worst <- ov[which.max(ov$max.rhat), ]
    esc(paste0(counts[["FAIL"]], " FAIL, ", counts[["CHECK"]], " CHECK and ", counts[["PASS"]], " PASS of ", nrow(ov),
               " cities. Highest worst-parameter R-hat: ", worst$city, " (", worst$max.rhat, ")."))
}

see.failing <- function(f, n.cities) {
    if (nrow(f) == 0) return("No parameter fails in any city.")
    common <- f[f$n.cities >= n.cities / 2, ]
    esc(paste0(n.of(nrow(f), "parameter"), verb(nrow(f), " fails", " fail"), " in at least one city; ", nrow(common), verb(nrow(common), " fails", " fail"), " in half the cities or more (",
               sum(grepl(TRANSMISSION.PATTERN, common$parameter)), " of them transmission parameters). Most frequent: ",
               f$parameter[1], " (", f$n.cities[1], " cities, highest R-hat ", f$max.rhat[1], ")."))
}

see.chains <- function(ch) {
    low <- ch[which.max(ch$gap.from.best), ]
    esc(paste0("Chain acceptance ranges ", min(ch$acceptance), " to ", max(ch$acceptance), ". ",
               if (low$gap.from.best > 0) paste0("Chain ", low$chain, " has the lowest mean log-likelihood, ",
                                                 low$gap.from.best, " below the best chain.") else ""))
}

see.log.density <- function(ld, summary) {
    r <- ld$rhat[ld$quantity == "log-posterior"]
    fit <- if (r <= 1.05) "the chains reach the same fit" else if (r <= 1.1) "the chains' fit differs slightly"
           else "the chains sit at different levels of fit"
    extra <- if (r <= 1.1 && summary$n.rhat.over.1.1 > 0)
        paste0(", while ", n.of(summary$n.rhat.over.1.1, "parameter"), verb(summary$n.rhat.over.1.1, " fails", " fail"), ": the chains fit equally well with different ",
               "parameter values, a sign that some parameters are not separately identified (METHODS 2.3)") else ""
    esc(paste0("Log-posterior R-hat ", r, ": ", fit, extra, "."))
}

see.loglik.plot <- function(ch, ld) {
    low <- ch[which.max(ch$gap.from.best), ]
    esc(paste0("Chain ", low$chain, " sits lowest (", low$gap.from.best, " below the best); log-posterior R-hat ",
               ld$rhat[ld$quantity == "log-posterior"], "."))
}

see.worst <- function(bp) {
    bad <- bp[bp$rhat > 1.1, ]
    if (nrow(bad) == 0) return(esc(paste0("No parameter has R-hat > 1.1. Worst: ", bp$parameter[1], " (", bp$rhat[1], ").")))
    n2 <- sum(bp$rhat > 2, na.rm = TRUE)
    esc(paste0(n.of(nrow(bad), "parameter"), verb(nrow(bad), " has", " have"), " R-hat > 1.1 (", sum(bad$transmission),
               " transmission), ", n2, verb(n2, " has", " have"), " R-hat > 2. Worst: ", bp$parameter[1], " (R-hat ", bp$rhat[1],
               "; classic ", bp$rhat.classic[1], ")."))
}

see.trace <- function(bp, n) {
    shown <- head(bp, n)
    esc(paste0("The ", nrow(shown), " worst parameters, R-hat ", min(shown$rhat), " to ", max(shown$rhat),
               " (classic ", min(shown$rhat.classic, na.rm = TRUE), " to ", max(shown$rhat.classic, na.rm = TRUE), ")."))
}

see.blocks.low <- function(bl) {
    if (nrow(bl) == 0) return("No block is below 0.05 in any chain.")
    esc(paste0(n.of(length(unique(bl$block)), "block"), " below 0.05 in at least one chain (chain", if (length(unique(bl$chain)) > 1) "s " else " ",
               paste(sort(unique(bl$chain)), collapse = ", "), "). Lowest: ", bl$block[1], " in chain ", bl$chain[1],
               " (", bl$acceptance[1], ")."))
}

see.accept.block <- function(ab)
    esc(paste0("Lowest block: ", names(ab)[1], " (", round(ab[[1]], 3), "); highest: ", names(ab)[length(ab)],
               " (", round(ab[[length(ab)]], 3), ")."))

see.accept.time <- function(ch)
    esc(paste0("Acceptance over the kept half, by chain: ", paste0("chain ", ch$chain, " ", ch$acceptance, collapse = ", "), "."))

see.leave.one <- function(lo, base) {
    if (is.null(lo)) return("Not computed (fewer than 3 chains).")
    first <- lo[1, ]
    if (first$max.rhat <= 1.1)
        return(esc(paste0("Leaving out chain ", first$chain.left.out, " brings the worst R-hat from ", base, " to ",
                          first$max.rhat, ": that chain alone is in a different place.")))
    if (first$drop.in.max.rhat >= 2 * max(lo$drop.in.max.rhat[2], 0.001))
        return(esc(paste0("Leaving out chain ", first$chain.left.out, " lowers the worst R-hat most (", base, " to ",
                          first$max.rhat, "), but it stays above 1.1: that chain contributes most, the others also disagree.")))
    esc(paste0("Leaving out any one chain changes the worst R-hat little (", min(lo$max.rhat), " to ", max(lo$max.rhat),
               "): no single chain is to blame."))
}

see.burn.in <- function(bi) {
    r <- setNames(bi$max.rhat, bi$share.removed)
    early <- drop.share(r[["0.5"]], r[["0"]])        # share of the excess R-hat removed by the burn
    late  <- drop.share(r[["0.75"]], r[["0.5"]])     # ... by removing a further 25%
    esc(paste0("Worst R-hat ", r[["0"]], " (whole chain), ", r[["0.5"]], " (second half), ", r[["0.75"]], " (last quarter). ",
               if (early >= 0.3) "It falls when the first half is removed, " else "It barely changes when the first half is removed, ",
               if (late >= 0.3) "and keeps falling at 75%: the kept half is still settling." else
                   if (r[["0.5"]] > 1.1) "and is flat and high after that: more burn-in will not help." else "and is flat after that."))
}

see.ident.types <- function(tt) {
    if (is.null(tt) || nrow(tt) == 0) return("No designed transmission combinations in this city.")
    txt <- paste0(tt$type, "s: median R-hat ", tt$median.rhat.combination, " vs worst part ", tt$median.max.rhat.parts,
                  collapse = "; ")
    ok <- all(c("level", "shape") %in% tt$type) && all(tt$median.rhat.combination[tt$type %in% c("level", "shape")] <= 1.1) &&
          any(tt$median.max.rhat.parts > IDENT.RHAT.FAIL)
    esc(paste0(txt, ".", if (ok) " Levels and shapes mix while their parts do not: the transmission parameters trade off along the redundant scale (METHODS 5.4)." else ""))
}

see.ident.v2 <- function(v2) {
    if (is.null(v2) || nrow(v2) == 0) return("None of these combinations could be built in this city.")
    id <- v2$type[v2$verdict == "IDENTIFIED"]
    esc(paste0(if (length(id)) paste0("IDENTIFIED: ", paste(id, collapse = ", "), ". ") else "No type is IDENTIFIED. ",
               "Best of each: ", paste0(v2$type, " ", v2$rhat.combination, collapse = ", "), "."))
}

see.ident.partner <- function(bp) {
    if (is.null(bp) || nrow(bp) == 0) return(paste0("Fewer than two parameters with R-hat > ", IDENT.PAIR.RHAT, "."))
    ok <- bp$rhat.combination <= IDENT.RHAT.OK
    alone <- bp$parameter[!ok]
    esc(paste0(sum(ok), " of ", nrow(bp), " parameters with R-hat > ", IDENT.PAIR.RHAT,
               " have a partner they mix with (combination R-hat <= ", IDENT.RHAT.OK, ").",
               if (length(alone)) paste0(" Without one (not part of a two-parameter ridge): ",
                                         paste(head(alone, 5), collapse = ", "), if (length(alone) > 5) ", ..." else "", ".") else ""))
}

see.ident.trace <- function(cmb, names)
    esc(paste0(paste0(names, " ", cmb$rhat.combination[match(names, cmb$combination)], " (worst part ",
                      cmb$max.rhat.parts[match(names, cmb$combination)], ")", collapse = "; "), "."))

see.cross.verdict <- function(cv) {
    if (is.null(cv) || nrow(cv) == 0) return("Not computed.")
    esc(paste0("IDENTIFIED by type: ", paste0(cv$type, " ", cv$IDENTIFIED, collapse = ", "), "."))
}

see.cross.partner <- function(cp) {
    if (is.null(cp) || nrow(cp) == 0) return("No best-partner pair repeats in 3 or more cities.")
    esc(paste0(n.of(nrow(cp), "pair"), verb(nrow(cp), " repeats", " repeat"), " in 3 or more cities. Most frequent: ", cp$pair[1], " (", cp$n.cities[1], " cities)."))
}


## 3. ONE CITY ----
# Loads the MCMC, computes everything for the city, builds its HTML, and frees the MCMC.
# Returns the small tables (for the overview and the CSVs) and the HTML.
TRACE.DESIGNED <- c("level.het", "shape.het.2022", "level.msm", "shape.race.other")   # METHODS 5.4

city.section <- function(loc, code, include.identifiability = TRUE, n.worst = 15, n.trace = 9) {
    city <- city.name(loc)
    message("== ", city)
    m <- tryCatch(load.mcmc.cached(loc, code), error = function(e) { message(city, ": ", e$message); NULL })
    if (is.null(m)) return(list(city = city, loc = loc, verdict = "NOT LOADED",
                                html = paste0("<section class='city' id='", esc(loc), "'><h2>", esc(city),
                                              "</h2><p class='none'>Not loaded.</p></section>")))

    d  <- mixing.stats(m)
    v  <- mixing.verdict(d$summary)
    lo <- leave.one.chain.out(d$samples)                      # METHODS 4.1
    bi <- burn.in.check(m)                                    # METHODS 4.2 (whole chain)
    id <- if (include.identifiability) identifiability.check(d$samples) else NULL
    m.kept <- burn.mcmc(m, d$summary$burn.keep)
    sm <- d$summary
    ab <- d$accept.by.block
    ab.table <- data.frame(block = names(ab), acceptance = round(as.numeric(ab), 3))

    # ---- A. Fit ----
    A <- paste0(
        item("Chains", HOW.TO.READ$chains, see.chains(d$chains), tbl(d$chains)),
        item("Log-likelihood and log-posterior: R-hat and ESS", HOW.TO.READ$log.density,
             see.log.density(d$log.density, sm), tbl(d$log.density)),
        item("Log-likelihood by chain", HOW.TO.READ$loglik.plot, see.loglik.plot(d$chains, d$log.density),
             img(function() {
                 ll <- d$loglik
                 matplot(ll, type = "l", lty = 1, col = seq_len(ncol(ll)), xlab = "saved draw (kept half)",
                         ylab = "log-likelihood", main = city)
                 legend("bottomright", legend = paste("chain", seq_len(ncol(ll))), col = seq_len(ncol(ll)), lty = 1, bty = "n")
             }, height = 500)))

    # ---- B. Parameters ----
    B <- paste0(
        item(paste0("Worst ", n.worst, " parameters"), HOW.TO.READ$worst, see.worst(d$by.parameter),
             tbl(head(d$by.parameter, n.worst))),
        item("Trace plots: worst-mixing parameters", HOW.TO.READ$trace, see.trace(d$by.parameter, n.trace),
             img(function() print(trace.plot(m.kept, var.names = head(d$by.parameter$parameter, n.trace),
                                             exact.var.names = TRUE)), height = 900)))

    # ---- C. Sampler ----
    C <- paste0(
        item("Blocks with acceptance below 0.05 in a chain", HOW.TO.READ$blocks.low, see.blocks.low(d$blocks.low),
             tbl(d$blocks.low)),
        item("Acceptance by block (lowest first)", HOW.TO.READ$accept.block, see.accept.block(ab),
             paste0(tbl(head(ab.table, 10)),
                    if (nrow(ab.table) > 10) paste0("<details><summary>All ", nrow(ab.table), " blocks</summary>",
                                                    tbl(ab.table[-(1:10), ]), "</details>") else "")),
        item("Acceptance over time, by chain", HOW.TO.READ$accept.time, see.accept.time(d$chains),
             img(function() print(acceptance.plot(m.kept)), height = 500)))

    # ---- D. Causes ----
    D <- paste0(
        item("Leave one chain out", HOW.TO.READ$leave.one, see.leave.one(lo, sm$max.rhat), tbl(lo)),
        item("Burn-in check", HOW.TO.READ$burn.in, see.burn.in(bi), tbl(bi)))

    # ---- E. Identifiability ----
    E <- ""; type.table <- NULL
    if (!is.null(id)) {
        cmb <- id$combinations
        tr  <- cmb[cmb$type %in% c("level", "shape", "product"), ]
        type.table <- if (nrow(tr)) do.call(rbind, lapply(c("level", "shape", "product"), function(tp) {
            x <- tr[tr$type == tp, ]
            if (nrow(x) == 0) return(NULL)
            data.frame(type = tp, n = nrow(x), median.rhat.combination = round(median(x$rhat.combination), 3),
                       median.max.rhat.parts = round(median(x$max.rhat.parts), 3),
                       n.identified = sum(x$verdict == "IDENTIFIED"), n.partly = sum(x$verdict == "PARTLY"))
        }))
        v2 <- cmb[!cmb$type %in% c("level", "shape", "product"), ]
        v2 <- v2[order(v2$type, v2$rhat.combination), ]
        v2 <- v2[!duplicated(v2$type), c("type", "combination", "rhat.combination", "max.rhat.parts", "verdict")]
        partners <- id$best.partner
        top.partners <- if (!is.null(partners)) {
            key <- paste(pmin(partners$parameter, partners$best.partner), pmax(partners$parameter, partners$best.partner))
            head(partners[!duplicated(key), c("parameter", "own.rhat", "best.partner", "partner.rhat", "form",
                                               "within.chain.cor", "rhat.combination")], 5)
        }
        shown <- intersect(TRACE.DESIGNED, names(id$combos))
        E <- paste0(
            item("Levels, shapes and products", HOW.TO.READ$ident.types, see.ident.types(type.table), tbl(type.table)),
            item("Other designed combinations: best of each type", HOW.TO.READ$ident.v2, see.ident.v2(v2),
                 tbl(v2, row.class = v2$verdict)),
            item("Best partners", HOW.TO.READ$ident.partner, see.ident.partner(partners), tbl(top.partners)),
            if (length(shown)) item("Trace plots: levels and shapes", HOW.TO.READ$ident.trace, see.ident.trace(cmb, shown),
                 img(function() {
                     par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
                     it <- unique(round(seq(1, dim(d$samples)[1], length.out = min(500, dim(d$samples)[1]))))
                     for (nm in shown) {
                         x <- combine(d$samples, id$combos[[nm]]$parts, id$combos[[nm]]$coefs)[it, , drop = FALSE]
                         matplot(it, x, type = "l", lty = 1, col = seq_len(ncol(x)), xlab = "saved draw (kept half)",
                                 ylab = "log scale", main = sprintf("%s (R-hat %.2f)", nm, cmb$rhat.combination[cmb$combination == nm]))
                     }
                 }, height = 750)) else "")
    }

    # ---- card ----
    key.numbers <- paste0("worst R-hat ", sm$max.rhat, " &middot; ", sm$n.rhat.over.1.1, " parameters &gt; 1.1 &middot; ",
                          "log-posterior R-hat ", sm$rhat.logpost, " &middot; lowest ESS ", sm$min.ess, " &middot; ",
                          "acceptance ", sm$accept.min.chain, "&ndash;", sm$accept.max.chain)
    part <- function(letter, title, body) paste0("<h3 id='", esc(loc), "-", letter, "'>", letter, ". ", title, "</h3>", body)
    letters.used <- c(A = "Fit", B = "Parameters", C = "Sampler", D = "Causes", E = "Identifiability")
    if (is.null(id)) letters.used <- letters.used[1:4]
    links <- paste0("<a href='#", esc(loc), "-", names(letters.used), "'>", names(letters.used), ". ", letters.used, "</a>",
                    collapse = " ")
    html <- paste0(
        "<section class='city' id='", esc(loc), "'>",
        "<h2>", esc(city), " <span class='pill ", tolower(v$verdict), "'>", v$verdict, "</span></h2>",
        "<p class='key'>", key.numbers, "</p>",
        if (nzchar(v$reasons)) paste0("<p class='reasons'>", esc(v$reasons), "</p>") else "",
        "<p class='links'>", links, " <a href='#top'>&uarr; top</a></p>",
        part("A", "Fit", A), part("B", "Parameters", B), part("C", "Sampler", C), part("D", "Causes", D),
        if (!is.null(id)) part("E", "Identifiability", E) else "",
        "</section>")

    rm(m, m.kept); invisible(gc())
    list(city = city, loc = loc, verdict = v$verdict, reasons = v$reasons, summary = sm, by.parameter = d$by.parameter,
         combinations = if (!is.null(id)) id$combinations, best.partner = id$best.partner, html = html)
}


## 4. THE REPORT ----
write.mixing.report <- function(locations = LOCATIONS, code = CALIBRATION.CODE, include.identifiability = TRUE,
                                folder = calibration.dir("mixing", code)) {
    locations <- unname(locations)
    res <- lapply(locations, city.section, code = code, include.identifiability = include.identifiability)
    names(res) <- sapply(res, `[[`, "city")
    ok <- res[sapply(res, function(r) !is.null(r$summary))]

    # overview (METHODS 3)
    if (length(ok) == 0) stop("No city could be loaded")
    empty <- ok[[1]]$summary; empty[1, ] <- NA                   # summary row of NAs, for cities not loaded
    overview <- do.call(rbind, lapply(res, function(r)
        cbind(data.frame(city = r$city, location = r$loc, verdict = r$verdict,
                         reasons = if (is.null(r$reasons)) "" else r$reasons, stringsAsFactors = FALSE),
              if (!is.null(r$summary)) r$summary else empty)))
    rownames(overview) <- NULL
    ov.cols <- c("city", "verdict", "reasons", "max.rhat", "n.rhat.over.1.1", "n.rhat.over.2", "rhat.logpost", "min.ess",
                 "accept.min.chain", "accept.max.chain", "min.block.accept")
    ov.show <- overview[, intersect(ov.cols, names(overview))]
    ov.show$city <- paste0("<a href='#", esc(overview$location), "'>", esc(overview$city), "</a>")

    failing <- params.failing.across.cities(lapply(ok, `[[`, "by.parameter"))

    # cross-city identifiability
    cross <- ""
    all.comb <- all.best <- NULL
    if (include.identifiability && length(ok)) {
        all.comb <- do.call(rbind, lapply(ok, function(r) if (!is.null(r$combinations)) cbind(city = r$city, r$combinations)))
        all.best <- do.call(rbind, lapply(ok, function(r) if (!is.null(r$best.partner)) cbind(city = r$city, r$best.partner)))
        cv <- if (!is.null(all.comb)) {
            t <- as.data.frame.matrix(table(all.comb$type, factor(all.comb$verdict, levels = c("IDENTIFIED", "PARTLY", "NOT", "NA"))))
            cbind(type = rownames(t), t)
        }
        cp <- if (!is.null(all.best)) {
            pair <- paste(pmin(all.best$parameter, all.best$best.partner), "<->", pmax(all.best$parameter, all.best$best.partner))
            n <- tapply(all.best$city, pair, function(x) length(unique(x)))
            n <- sort(n[n >= 3], decreasing = TRUE)
            if (length(n)) data.frame(pair = names(n), n.cities = as.integer(n))
        }
        cross <- paste0("<section id='cross'><h2>Identifiability across cities</h2>",
                        item("Verdicts by type", HOW.TO.READ$cross.verdict, see.cross.verdict(cv), tbl(cv)),
                        item("Best partners repeated across cities", HOW.TO.READ$cross.partner, see.cross.partner(cp), tbl(cp)),
                        "</section>")
    }

    # settings
    info <- tryCatch(get.calibration.info(code), error = function(e) NULL)
    th <- MIXING.THRESHOLDS
    settings <- data.frame(
        setting = c("calibration code", "report date", "cities", "share of each chain used (last part)", "chains",
                    "iterations per chain", "thin", "verdict cut-offs", "identifiability"),
        value = c(code, format(Sys.time(), "%Y-%m-%d %H:%M"), length(locations), MIXING.BURN.KEEP,
                  if (!is.null(info)) info$n.chains else NA, if (!is.null(info)) info$n.iter else NA,
                  if (!is.null(info)) info$thin else NA,
                  paste0("R-hat ", th$rhat.check, " / ", th$rhat.fail, "; ESS ", th$ess.min, "; acceptance ", th$accept.low,
                         " / ", th$accept.high, "; block ", th$block.accept.low),
                  if (include.identifiability) paste0("included (part fails > ", IDENT.RHAT.FAIL, "; mixes <= ",
                                                      IDENT.RHAT.OK, "; pairs among R-hat > ", IDENT.PAIR.RHAT, ")") else "not included"))

    stats.table <- STATISTICS.EXPLAINED
    names(stats.table) <- c("statistic", "how it is calculated", "how to read it", "advantages", "limitations", "METHODS")

    # sidebar: one link per city, coloured by verdict
    nav <- paste0("<nav><a class='navtop' href='#top'>Top</a><a href='#overview'>Overview</a>",
                  paste0("<a class='", tolower(gsub(" ", "", overview$verdict)), "' href='#", esc(overview$location), "'>",
                         esc(overview$city), "</a>", collapse = ""),
                  if (nzchar(cross)) "<a href='#cross'>Across cities</a>" else "", "<a href='#refs'>References</a></nav>")
    counts <- table(factor(overview$verdict, levels = c("FAIL", "CHECK", "PASS", "NOT LOADED")))
    pills <- paste0("<span class='pill ", tolower(gsub(" ", "", names(counts))), "'>", names(counts), ": ", counts, "</span>",
                    collapse = " ")

    html <- paste0(
        "<!doctype html><html><head><meta charset='utf-8'><meta name='viewport' content='width=device-width, initial-scale=1'>",
        "<title>Mixing report ", esc(code), "</title><style>", REPORT.CSS, "</style></head><body>", nav,
        "<main><a id='top'></a><h1>MCMC mixing report</h1><p class='key'>", esc(code), "</p><p>", pills, "</p>",
        "<section><h2>Methods summary</h2><ul>", paste0("<li>", REPORT.INTRO, "</li>", collapse = ""), "</ul>",
        "<details><summary>Statistics explained (classic R-hat, rank-normalized split R-hat, log-posterior R-hat, ESS, acceptance)</summary>",
        tbl(stats.table), "</details></section>",
        "<section><h2>Run settings</h2>", tbl(settings), "</section>",
        "<section id='overview'><h2>Overview</h2>",
        item("All cities", HOW.TO.READ$overview, see.overview(overview[!is.na(overview$max.rhat), ]),
             tbl(ov.show, row.class = gsub(" ", "", overview$verdict), html.cols = "city")),
        item("Parameters failing across cities", HOW.TO.READ$failing, see.failing(failing, length(ok)), tbl(failing)),
        "</section>",
        paste(sapply(res, `[[`, "html"), collapse = ""),
        cross,
        "<section id='refs'><h2>References</h2><ol>",
        "<li>Vehtari A, Gelman A, Simpson D, Carpenter B, B&uuml;rkner P-C. Rank-normalization, folding, and localization: an improved R-hat for assessing convergence of MCMC. Bayesian Analysis 2021;16(2):667-718. doi:10.1214/20-BA1221</li>",
        "<li>Gelman A, Rubin DB. Inference from iterative simulation using multiple sequences. Statistical Science 1992;7(4):457-472. doi:10.1214/ss/1177011136</li>",
        "<li>Geyer CJ. Practical Markov chain Monte Carlo. Statistical Science 1992;7(4):473-483. doi:10.1214/ss/1177011137</li>",
        "<li>Roberts GO, Gelman A, Gilks WR. Weak convergence and optimal scaling of random walk Metropolis algorithms. Annals of Applied Probability 1997;7(1):110-120. doi:10.1214/aoap/1034625254</li>",
        "</ol></section></main></body></html>")

    # files
    stamp <- format(Sys.time(), "%Y%m%d_%H%M")
    f <- function(what, ext) file.path(folder, paste0(what, "_", code, "_", stamp, ".", ext))
    writeLines(html, f("mixing_report", "html"))
    write.annotated.csv(overview, f("mixing_overview", "csv"))
    write.annotated.csv(do.call(rbind, lapply(ok, function(r) cbind(city = r$city, r$by.parameter))), f("mixing_parameters", "csv"))
    if (!is.null(all.comb)) write.annotated.csv(all.comb, f("identifiability_combinations", "csv"))
    if (!is.null(all.best)) write.annotated.csv(all.best, f("identifiability_best_partner", "csv"))
    message("Report saved: ", f("mixing_report", "html"))
    invisible(list(overview = overview, results = res))
}

REPORT.CSS <- "
:root { --bg:#fff; --fg:#1d1d1f; --muted:#666; --line:#ddd; --head:#f4f4f6; --card:#fafafa;
        --pass:#e3f4e6; --check:#fff4d6; --fail:#fbe0e0; --na:#eee; --link:#0b5cad; }
@media (prefers-color-scheme: dark) {
  :root { --bg:#1b1b1d; --fg:#e8e8ea; --muted:#aaa; --line:#3a3a3d; --head:#2a2a2d; --card:#232326;
          --pass:#1f3a25; --check:#3d3418; --fail:#442222; --na:#333; --link:#7db4ff; } }
body { background:var(--bg); color:var(--fg); font:14px/1.5 -apple-system, Segoe UI, Helvetica, Arial, sans-serif; margin:0; }
a { color:var(--link); }
nav { position:fixed; top:0; left:0; bottom:0; width:170px; overflow-y:auto; padding:16px 10px;
      border-right:1px solid var(--line); background:var(--bg); }
nav a { display:block; padding:4px 8px; margin:2px 0; border-radius:6px; text-decoration:none; color:var(--fg); }
nav a.pass { background:var(--pass); } nav a.check { background:var(--check); } nav a.fail { background:var(--fail); }
nav a.notloaded { background:var(--na); } nav a.navtop { font-weight:600; }
main { margin-left:200px; max-width:1150px; padding:20px 20px 60px; }
@media (max-width: 900px) { nav { position:static; width:auto; border-right:none; border-bottom:1px solid var(--line); }
  nav a { display:inline-block; } main { margin-left:0; padding:16px; } }
h1 { margin:0 0 4px; } h2 { margin-top:36px; border-bottom:1px solid var(--line); padding-bottom:4px; }
h3 { margin-top:26px; font-size:16px; } h4 { margin:18px 0 4px; font-size:14px; }
section.city { background:var(--card); border:1px solid var(--line); border-radius:10px; padding:4px 18px 18px; margin-top:28px; }
.key { color:var(--muted); margin:2px 0; } .reasons { margin:2px 0; } .links a { margin-right:10px; }
.how, .see { margin:2px 0; } .see { color:var(--muted); } .none { color:var(--muted); font-style:italic; }
.scroll { overflow-x:auto; } table { border-collapse:collapse; font-size:12.5px; margin:6px 0; }
th, td { border:1px solid var(--line); padding:4px 8px; text-align:left; vertical-align:top; }
th { background:var(--head); cursor:help; }
tr.pass td, tr.identified td { background:var(--pass); } tr.check td, tr.partly td { background:var(--check); }
tr.fail td, tr.not td { background:var(--fail); } tr.notloaded td { background:var(--na); }
.pill { display:inline-block; padding:2px 10px; border-radius:10px; font-size:13px; font-weight:600; }
.pill.pass { background:var(--pass); } .pill.check { background:var(--check); } .pill.fail { background:var(--fail); }
.pill.notloaded { background:var(--na); }
details { margin:6px 0; } summary { cursor:pointer; color:var(--link); }
img { max-width:100%; background:#fff; border-radius:4px; }"
