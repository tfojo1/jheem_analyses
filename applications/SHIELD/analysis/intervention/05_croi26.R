# ****************************************************************************************************
# SHIELD / Doxy-PEP -- MANUSCRIPT OUTPUTS
# ****************************************************************************************************
#

# CONFIGURATION ------------------------------------------------------
if(1==1){
    source('../jheem_analyses/commoncode/locations_of_interest.R')
    # source('../jheem_analyses/applications/SHIELD/shield_specification.R')
    source('../jheem_analyses/applications/SHIELD/analysis/intervention/intervention_helper_functions.R')
    
    print(paste("Root directory is set to: ",ROOT.DIR))
    
    # Set the calibration once. The folders and the four result arrays follow from it.
    CALIB.NAME <- "calib.9.23.stage3.pk"
    FIG.DIR    <- shield.fig.path(CALIB.NAME,   create = TRUE)
    TABLE.DIR  <- shield.table.path(CALIB.NAME, create = TRUE)
    print(paste("Figures/Tables will be written to: ",FIG.DIR," **/tables/"))
    
    # Loading raw and calculated results:
    results <- load.shield.results(CALIB.NAME)
    #
    total_raw_results  <- results$total_raw
    total_calc_results <- results$total_calc
    sex_raw_results    <- results$sex_raw
    sex_calc_results   <- results$sex_calc
    
    print(paste("Results are loaded"))
}
# FIGURE SETTINGS ------------------------------------------------------
{
    CCRIT.MS   <- "doxy.cov.20"                        # policy coverage level
    EVAL.YEAR  <- "2030"                               # policy evaluation horizon
    BASE.YEAR  <- "2022"
    COVERAGE.LEVELS <- paste0("doxy.cov.", seq(5, 50, 5))
    COVERAGE.LEVELS <- paste0("doxy.cov.", c(10,25,50))
    
    .ccrit     <- as.integer(sub("doxy\\.cov\\.", "", CCRIT.MS))
    
    .pos <- function(x) ifelse(!is.na(x) & x > 0, x, NA_real_)
    
    .label_layer <- function() {
        if (requireNamespace("ggrepel", quietly = TRUE))
            ggrepel::geom_text_repel(aes(label = location), size = 3.2)
        else geom_text(aes(label = location), size = 3.2, hjust = -0.15, vjust = -0.4)
    }
    
    
    HEAT.MIDPOINT <- 50      # policy target; also the upper break between bands
    HEAT.LABEL    <- "black" # cell number colour. NULL = white or black per cell, by contrast
    # Font sizes (pt) for the heatmap figure, one per text element
    HEAT.FONT <- list(
        base         = 6.4,   # spacing and margins scale from this
        title        = 6.4,   # panel titles "A: ..." and "B: ..."
        axis.title   = 6.4,   # "Doxy-PEP Coverage ..."
        axis.text    = 6.4,   # city names and coverage levels
        legend.title = 6.4,
        legend.text  = 5.1,
        cell         = 5.6    # numbers inside the tiles
    )
    
    MSAS <- names(SHIELD.TEN.MSAS)
    PRINT.VERSION = T #
}

# ****************************************************************************************************
# SHARED INPUTS -- the coverage x location surfaces used by Figures 3 and 3a ----
# ****************************************************************************************************
{
    tbl.cov.vs.noint <- make_multi_location_table(
        data          = results,
        locations     = MSAS,
        outcomes      = c("pct_incidence_averted"),
        interventions = COVERAGE.LEVELS,
        years         = EVAL.YEAR,
        stat.type     = "median",
        save          = F
    )
}


# ****************************************************************************************************
# FIGURE -- IMPACT HEATMAPS ----
# ****************************************************************************************************
{
    
    HEAT.FRAME <- "reduction"          # "change" or "reduction"
    
    # Negate every value column, leaving the identifier columns alone. Value
    # columns are the ones matching the builder's <outcome>_<intervention>_<year>
    # pattern -- the same pattern plot_coverage_heatmap() uses to find them.
    .flip.values <- function(tbl, pattern = "^(.*)_doxy\\.cov\\.(\\d+)_(\\d+)$") {
        hit <- grepl(pattern, names(tbl))
        if (!any(hit))
            stop("HEAT.FRAME = 'change': no column matched '", pattern, "', so ",
                 "nothing would be flipped and the figure would silently stay on the ",
                 "reduction scale. Has col.pattern changed?")
        tbl[hit] <- lapply(tbl[hit], function(v) {
            num <- suppressWarnings(as.numeric(v))
            if (all(is.na(num)) && !all(is.na(v)))
                stop("HEAT.FRAME = 'change': a value column is not numeric and cannot ",
                     "be sign-flipped. A median.ci table carries '[lower-upper]' ",
                     "strings -- use stat.type = 'median' for this figure.")
            -num
        })
        tbl
    }
    
    # Everything that has to move together when the sign convention flips. Under
    # "change" the breaks become c(-49, 0) and the colours reverse, so the band
    # MEANINGS are unchanged: blue = target met, tint = benefit short of target,
    # red = worse off. band.reverse = c(1, 2) because BOTH lower bands now sit
    # below the neutral value and must darken towards their extreme.
    .hf <- if (HEAT.FRAME == "change")
        list(breaks   = c(-HEAT.MIDPOINT, 0),
             colours  = rev(SHIELD.HEAT.COLS),
             reverse  = c(1L, 2L),
             midpoint = -HEAT.MIDPOINT,
             better   = FALSE,
             lab      = "% Change in Incidence")
    else if (HEAT.FRAME == "reduction")
        list(breaks   = c(0, HEAT.MIDPOINT),
             colours  = SHIELD.HEAT.COLS,
             reverse  = 1L,
             midpoint = HEAT.MIDPOINT,
             better   = TRUE,
             lab      = "% Reduction in Incidence")
    else stop("HEAT.FRAME must be 'change' or 'reduction', not '", HEAT.FRAME, "'.")
    
    # helper code to build the heatmap from each table
    .mk.heat <- function(tbl, strat, ttl){
        plot_coverage_heatmap(
            if (HEAT.FRAME == "change") .flip.values(tbl) else tbl,
            locations    = MSAS,
            subgroup     = strat,
            midpoint     = .hf$midpoint,
            band.breaks  = .hf$breaks,
            band.colours = .hf$colours,
            band.reverse = .hf$reverse,
            higher.is.better = .hf$better,
            fill.style   = "banded",
            limits       = c(0, 100),
            order.rows   = "alpha",
            label.colour = HEAT.LABEL,
            squish.marks = "always",        # <- identical labels on every panel
            title        = ttl,
            fill.lab     = .hf$lab,
            legend.dir   = "horizontal",
            fixed.aspect = FALSE,
            label.suffix="%",
            # font sizes, all from HEAT.FONT above
            base.size         = HEAT.FONT$base,
            title.size        = HEAT.FONT$title,
            axis.title.size   = HEAT.FONT$axis.title,
            axis.text.size    = HEAT.FONT$axis.text,
            legend.title.size = HEAT.FONT$legend.title,
            legend.text.size  = HEAT.FONT$legend.text,
            # cell text is set in mm, not pt; .pt converts
            label.size        = HEAT.FONT$cell / ggplot2::.pt) +
            # spell out the abbreviated city names on the y-axis. Replaces the
            # function's own y scale, so expand = c(0, 0) is repeated here.
            scale_y_discrete(expand = c(0, 0),
                             labels = c(LA = "Los Angeles", NYC = "New York City",
                                        Atlanta = "Atlanta", Baltimore = "Baltimore",
                                        Chicago = "Chicago", Houston = "Houston",
                                        Miami = "Miami", Philadelphia = "Philadelphia",
                                        Phoenix = "Phoenix", Seattle = "Seattle")) +
            # all text in black. axis.text is set on its own because the theme
            # gives it its own dark grey, which `text` does not override
            theme(text      = element_text(colour = "black"),
                  axis.text = element_text(colour = "black"))
    }
    
    p.heat.1 <- .mk.heat(tbl.cov.vs.noint,          "msm",   "A: Projected Incidence Reduction \namong MSM by 2030 \nwith Doxy-PEP vs no Doxy-PEP")
    p.heat.2 <- .mk.heat(tbl.cov.vs.noint,          "Total", "B: Projected Incidence Reduction \namong Total Population by 2030 \nwith Doxy-PEP vs no Doxy-PEP")
    
    .no.y.heat <- theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
    .no.x.heat <- theme(axis.title.x = element_blank(), axis.text.x = element_blank())
    
    # One shared colourbar instead of one per panel. The "+" before plot_layout() is
    # load-bearing: without it the assignment ends at the closing paren, plot_layout()
    # becomes a separate throwaway statement, and each panel keeps its own legend.
    # The two guides merge only because both panels are built with the same limits,
    # band.breaks, colours, fill.lab and squish.marks = "always".
    fig.heatmap <- (p.heat.1 + p.heat.2+.no.y.heat ) +
        plot_layout(guides = "collect") &
        theme(legend.position = "bottom");fig.heatmap
    
    ggsave(file.path(FIG.DIR, "fig_CROI.png"), fig.heatmap,
           width = 4, height = 4, dpi = 300, bg = "white")
 
}

# ****************************************************************************************************
# TABLE S1 -- DOXY-PEP IMPACT AT A SINGLE COVERAGE LEVEL, BY POPULATION ----
# ****************************************************************************************************
# WHAT   One row per MSA x population (Total / MSM / women / heterosexual men):
#        incident counts and incidence rates in BASE.YEAR and EVAL.YEAR under
#        `noint` and under CCRIT.MS, then the three impact metrics at EVAL.YEAR.
#        The numeric backing for Figure 4.
# INPUT  results, MSAS; medians only -- this is a supporting table, no CrI.
# OUTPUT tables/S1-impact-cov<c>-<yr>.csv  (object `final2`)
# WARNING col.labels.t2 is assigned POSITIONALLY, over names(tbl2)[-3]. Add or
#        reorder anything in t2.outcomes1 / t2.outcomes2 and every label shifts
#        silently -- check the CSV header row after any change here.

{
    # Doxy-PEP impact across the ten MSAs at a single coverage level, for the total 
    
    t2.outcomes1 <- c(
        # ---- BLOCK 0: KEY outcomes ----
        "incidence",
        "rate_incidence_per_pop"
    )
    
    tbl2.impact1 <- make_multi_location_table(
        data            = results,
        locations       = MSAS,
        outcomes        = t2.outcomes1,
        interventions   = c("noint",CCRIT.MS),
        filter.by.strat = c("Total", "msm","female","heterosexual_male"),
        years           = c(BASE.YEAR, EVAL.YEAR),
        stat.type       = "median",
        digits = c(.default                       = 0,
                   pct_incidence_averted          = 1,
                   pct_cum_incidence_averted      = 1,
                   rate_incidence_averted_per_pop = 1)
    )
    
    t2.outcomes2 <- c(
        # ---- BLOCK A: single year. RELATIVE and ABSOLUTE effect, no efficiency ----
        "pct_incidence_averted",
        "rate_incidence_averted_per_pop",
        # ---- BLOCK B: cumulative from baseline.year. Burden prevented, and the
        #      antibiotic it cost. Efficiency lives HERE and only here ----
        "num_cum_incidence_averted"
        # "pct_cum_incidence_averted",
        # "rate_cum_incidence_averted_ppy_doxy"
    )
    
    tbl2.impact2 <- make_multi_location_table(
        data            = results,
        locations       = MSAS,
        outcomes        = t2.outcomes2,
        interventions   = CCRIT.MS,
        filter.by.strat = c("Total", "msm","female","heterosexual_male"),
        years           = EVAL.YEAR,
        stat.type       = "median",
        digits = c(.default                       = 0,
                   pct_incidence_averted          = 1,
                   pct_cum_incidence_averted      = 1,
                   rate_incidence_averted_per_pop = 1)
    )
    
    # column names come back as <outcome>_<intervention>_<year>
    # .t2sfx is used only by the commented-out column reorder further down; it is
    # kept so that line still works if you re-enable it.
    .t2sfx <- paste0("_", CCRIT.MS, "_", EVAL.YEAR)
    .t2cov <- sub("doxy\\.cov\\.", "", CCRIT.MS)
    
    tbl2<-left_join(tbl2.impact1,tbl2.impact2,by = c("location","subgroup","stat"))
    t2.outcomes<-c(t2.outcomes1,t2.outcomes2)
    
    
    col.labels.t2 <- stats::setNames(
        c("MSA", "Population",
          paste0("Incidence ", BASE.YEAR, ", No Intervention"),
          paste0("Incidence ", EVAL.YEAR, ", No Intervention"),
          paste0("Incidence ", BASE.YEAR, ", Doxy Coverage ",.t2cov,"%"),
          paste0("Incidence ", EVAL.YEAR, ", Doxy Coverage ",.t2cov,"%"),
          paste0("Incidence Rate (per 100,000 population) ", BASE.YEAR, ", No Intervention"),
          paste0("Incidence Rate (per 100,000 population) ", EVAL.YEAR, ", No Intervention"),
          paste0("Incidence Rate (per 100,000 population) ", BASE.YEAR, ", Doxy Coverage ",.t2cov,"%"),
          paste0("Incidence Rate (per 100,000 population) ", EVAL.YEAR, ", Doxy Coverage ",.t2cov,"%"),
          
          
          paste0("Incidence averted (%), ", EVAL.YEAR),
          paste0("Incidence averted per 100,000 population, ", EVAL.YEAR),
          paste0("Cumulative infections averted, 2022-", EVAL.YEAR)
          # paste0("Cumulative incidence averted (%), 2022-", EVAL.YEAR),
          # paste0("Cumulative infections averted per 1000 doxy person-years, 2022-", EVAL.YEAR)),
        ),
        #
        names(tbl2)[-3]
    )
    
    hit2 <- names(tbl2) %in% names(col.labels.t2)
    final2 <- tbl2[names(tbl2)[hit2]]
    names(final2) <- col.labels.t2[names(final2)]
    
    # keep the block order regardless of the order the builder returned them in
    # final2 <- final2[col.labels.t2[c("location", "subgroup", paste0(t2.outcomes, .t2sfx))]]
    final2$Population <- factor(final2$Population, levels = c("Total", "msm","female","heterosexual_male"))
    final2 <- final2[order(final2$Population,final2$MSA),]
    final2
    
    if(PRINT.VERSION) {
        final2<-.remove.duplicate.rows(final2)
        file = paste0(TABLE.DIR, "S1-impact-cov", .t2cov, "-", EVAL.YEAR, ".csv")
        write.csv(final2,
                  file =file,
                  row.names = FALSE)
        print(paste0(file ," generated"))
    }
}
