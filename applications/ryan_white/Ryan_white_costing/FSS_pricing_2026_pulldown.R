library(readxl)
library(dplyr)
library(stringr)
library(tidyr)

fss <- read_excel("applications/ryan_white/Ryan_white_costing/vaFssPharmPrices.xlsx", sheet = "Prices") %>%
    mutate(
        Price = as.numeric(Price),
        Generic = str_to_upper(Generic),
        TradeName = str_to_upper(TradeName)
    )

## ---- Table 1 (Oxford Academic 372143954): drugs/regimens by core class, ----
## ---- with prevalence among ARV-naive individuals initiating ART in 2019-2020 (N = 293) ----
## Regimens named in Table 1 are priced as-is. Rows with an unspecified backbone
## (plain "Dolutegravir", "Raltegravir", "Atazanavir", "Doravirine", plain
## remainder of "Multicore") are filled in below using the DHHS Adult/Adolescent
## ARV Guidelines' Recommended Initial Regimens (clinicalinfo.hiv.gov), flagged
## as assumptions rather than treated as Table-1-derived.

art_keywords <- c(
    "BIKTARVY", "TRIUMEQ", "DOVATO", "GENVOYA", "STRIBILD", "SYMTUZA",
    "ODEFSEY", "COMPLERA", "ATRIPLA", "JULUCA", "DESCOVY",
    "DOLUTEGRAVIR", "DARUNAVIR", "RITONAVIR", "ATAZANAVIR", "RALTEGRAVIR",
    "DORAVIRINE", "ABACAVIR", "LAMIVUDINE", "TENOFOVIR", "EMTRICITABINE"
)

art_prices_df <- fss %>%
    filter(str_detect(Generic, str_c(art_keywords, collapse = "|")) |
               str_detect(TradeName, str_c(art_keywords, collapse = "|"))) %>%
    dplyr::select(NDCWithDashes, Generic, TradeName, PackageDescription, PriceType, Price,
                  PriceStartDate, PriceStopDate) %>%
    arrange(Generic, PriceType, Price)

art_prices_df <- art_prices_df %>%
    mutate(component = coalesce(Generic, TradeName))

fss_pool   <- art_prices_df %>% filter(PriceType == "FSS")

floor_pool <- art_prices_df %>%
    filter(PriceType %in% c("Big4", "NC"),
           NDCWithDashes %in% fss_pool$NDCWithDashes)

floor_best <- floor_pool %>%
    group_by(component) %>%
    slice_min(Price, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    dplyr::select(component, NDCWithDashes, floor_price = Price, floor_type = PriceType)

matched <- floor_best %>%
    left_join(fss_pool %>% dplyr::select(NDCWithDashes, ceiling_price = Price),
              by = "NDCWithDashes")

fss_only <- fss_pool %>%
    filter(!component %in% floor_pool$component) %>%
    group_by(component) %>%
    slice_min(Price, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(component, NDCWithDashes,
              floor_price = Price, floor_type = "FSS", ceiling_price = Price)

component_prices <- bind_rows(matched, fss_only) %>%
    arrange(component)

## Only DTG, ABC, 3TC, DRV, RTV are ever summed manually below (the one
## non-FDC regimen in Table 1: "DTG and DRV" under Multicore). Everything
## else in the table is priced as a fixed-dose product via get_fdc_price().
code_keywords <- c(
    DTG = "DOLUTEGRAVIR", ABC = "ABACAVIR", `3TC` = "LAMIVUDINE",
    DRV = "DARUNAVIR", RTV = "RITONAVIR",
    ATZ = "ATAZANAVIR", RAL = "RALTEGRAVIR", DOR = "DORAVIRINE"
)

match_code <- function(comp) {
    hits <- names(code_keywords)[sapply(code_keywords, function(k) str_detect(comp, fixed(k)))]
    if (length(hits) == 0) NA_character_ else hits[1]
}

adult_strength <- c(
    DTG = "DOLUTEGRAVIR 50MG TAB",
    ABC = "ABACAVIR SO4 300MG TAB",
    `3TC` = "LAMIVUDINE 300MG TAB",
    DRV = "DARUNAVIR ETHANOLATE 800MG TAB",
    RTV = "RITONAVIR 100MG TAB",
    ATZ = "ATAZANAVIR SO4 300MG CAP",
    RAL = "RALTEGRAVIR 600MG TAB",
    DOR = "DORAVIRINE 100MG TAB"
)

component_by_code <- component_prices %>%
    mutate(code = sapply(component, match_code)) %>%
    filter(!is.na(code), component == adult_strength[code]) %>%
    group_by(code) %>%
    slice_min(floor_price, n = 1, with_ties = FALSE) %>%
    ungroup()

missing_codes <- setdiff(names(code_keywords), component_by_code$code)
if (length(missing_codes) > 0) {
    stop("Missing component prices for: ", paste(missing_codes, collapse = ", "))
}

get_fdc_price <- function(keyword) {
    rows <- art_prices_df %>%
        filter(str_detect(TradeName, fixed(keyword)),
               !str_detect(TradeName, "PD$|PEDIATRIC"),
               !str_detect(Generic, "SUSP|SUSPENSION"))
    tibble(
        floor_price   = rows %>% filter(PriceType == "Big4") %>%
            slice_min(Price, n = 1, with_ties = FALSE) %>% pull(Price),
        ceiling_price = rows %>% filter(PriceType == "FSS") %>%
            slice_min(Price, n = 1, with_ties = FALSE) %>% pull(Price)
    )
}

combo_price <- function(codes) {
    component_by_code %>%
        filter(code %in% codes) %>%
        summarise(floor_price = sum(floor_price), ceiling_price = sum(ceiling_price))
}

## TDF/FTC (generic Truvada) -- floor and ceiling are genuinely different NDCs,
## so this can't go through get_fdc_price()'s single-NDC-per-PriceType logic.
tdf_ftc_price <- tibble(
    floor_price = art_prices_df %>%
        filter(Generic == "EMTRICITABINE 200MG/TENOFOVIR DISOPROXIL FUMARATE 300MG TAB",
               PriceType == "NC") %>%
        slice_min(Price, n = 1, with_ties = FALSE) %>% pull(Price),
    ceiling_price = art_prices_df %>%
        filter(Generic == "EMTRICITABINE 200MG/TENOFOVIR DISOPROXIL FUMARATE 300MG TAB",
               PriceType == "FSS") %>%
        slice_min(Price, n = 1, with_ties = FALSE) %>% pull(Price)
)

## combine a manually-priced anchor drug (via combo_price codes) with a
## fixed-dose backbone product (Descovy for TAF/FTC, tdf_ftc_price for TDF/FTC)
combined_price <- function(anchor_codes, backbone_price) {
    anchor <- combo_price(anchor_codes)
    tibble(
        floor_price   = anchor$floor_price   + backbone_price$floor_price,
        ceiling_price = anchor$ceiling_price + backbone_price$ceiling_price
    )
}

taf_ftc_price <- get_fdc_price("DESCOVY")

## ---- Table 1 naive-population fractions ----
## Core class % are of the full ARV-naive column (N = 293).
## Drug/regimen % within a class are of that class's bold total (per table footnote c).
## Italicized regimen % are a further subset of the drug row directly above (footnote d).
insti_naive   <- 0.96
pi_naive      <- 0.01
nnrti_naive   <- 0.01
multicore_naive <- 0.02

table1_regimens <- tribble(
    ~art_class,      ~regimen,                        ~type,   ~keyword,    ~naive_frac,
    "INSTI-based",   "BIC/TAF/FTC (Biktarvy)",        "fdc",   "BIKTARVY",  insti_naive * 0.87,
    "INSTI-based",   "DTG/ABC/3TC (Triumeq)",         "fdc",   "TRIUMEQ",   insti_naive * 0.11 * 0.32,
    "INSTI-based",   "DTG/3TC (Dovato)",              "fdc",   "DOVATO",    insti_naive * 0.11 * 0.10,
    "INSTI-based",   "EVG/c/TAF/FTC (Genvoya)",       "fdc",   "GENVOYA",   insti_naive * 0.02,
    "INSTI-based",   "EVG/c/TDF/FTC (Stribild)",      "fdc",   "STRIBILD",  insti_naive * 0.00,
    "PI-based",      "DRV/c/TAF/FTC (Symtuza)",       "fdc",   "SYMTUZA",   pi_naive * 1.00 * 0.75,
    "NNRTI-based",   "RPV/TAF/FTC (Odefsey)",         "fdc",   "ODEFSEY",   nnrti_naive * 1.00,
    "NNRTI-based",   "RPV/TDF/FTC (Complera)",        "fdc",   "COMPLERA",  nnrti_naive * 0.00,
    "NNRTI-based",   "EFV/TDF/FTC (Atripla)",         "fdc",   "ATRIPLA",   nnrti_naive * 0.00,
    "Multicore",     "DTG/RPV (Juluca)",              "fdc",   "JULUCA",    multicore_naive * 1.00 * 0.17,
    "Multicore",     "DTG + DRV/r (multicore)",       "combo", NA,          multicore_naive * 1.00 * 0.33
)

regimen_prices <- table1_regimens %>%
    rowwise() %>%
    mutate(
        prices = list(
            if (type == "fdc") get_fdc_price(keyword) else combo_price(c("DTG", "DRV", "RTV"))
        )
    ) %>%
    unnest(prices) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(median_price = median(c(floor_price, ceiling_price))) %>%
    ungroup() %>%
    dplyr::select(art_class, regimen, naive_frac, floor_price, median_price, ceiling_price)

regimen_prices

## naive-population share not modeled: unspecified-backbone rows in Table 1
## (plain Dolutegravir, Raltegravir, Atazanavir, Doravirine, plain "INSTI and
## other core drug" beyond the DTG+DRV and Juluca rows named above)
uncovered_frac <- 1 - sum(regimen_prices$naive_frac)
uncovered_frac

## ---- weighted annual ART cost among ARV-naive initiators, renormalized to ----
## ---- the population fraction actually covered by named regimens ----
art_cost_naive <- regimen_prices %>%
    summarise(
        coverage_pct = sum(naive_frac) * 100,
        low    = sum(floor_price   * naive_frac) / sum(naive_frac) * 12,
        median = sum(median_price  * naive_frac) / sum(naive_frac) * 12,
        high   = sum(ceiling_price * naive_frac) / sum(naive_frac) * 12
    )

art_cost_naive

## ---- ASSUMED backbone for the ~1% "INSTI and other core drug" remainder ----
## (Multicore 2% x 100% x (1 - 0.33 DTG+DRV - 0.17 Juluca) = 1.0%). Not stated
## in Table 1. Both named multicore rows (DTG+DRV, DTG/RPV/Juluca) are
## dual-therapy: INSTI + core drug, no NRTI backbone. DHHS guidelines
## (clinicalinfo.hiv.gov) list "DRV/r once daily plus RAL twice daily (CI)" as
## a real two-drug regimen for when ABC/TAF/TDF cannot be used -- the same
## dual-core pattern as the two named rows. DTG + ATV/r is NOT listed anywhere
## in the guidelines as a standalone pairing, so it is kept only as a
## secondary sensitivity comparator, not the primary guess.
multicore_other_naive <- multicore_naive * 1.00 * (1 - 0.33 - 0.17)

assumed_candidates <- tribble(
    ~regimen,                                            ~codes,
    "RAL + DRV/r (assumed, DHHS-listed dual regimen)",   list(c("RAL", "DRV", "RTV")),
    "DTG + ATV/r (assumed, sensitivity only)",           list(c("DTG", "ATZ", "RTV"))
) %>%
    rowwise() %>%
    mutate(prices = list(combo_price(codes[[1]]))) %>%
    unnest(prices) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(median_price = median(c(floor_price, ceiling_price))) %>%
    ungroup() %>%
    dplyr::select(regimen, floor_price, median_price, ceiling_price)

assumed_candidates

## point estimate = RAL + DRV/r (DHHS-listed dual regimen); DTG + ATV/r shown above for comparison
assumed_multicore_other <- assumed_candidates %>%
    filter(regimen == "RAL + DRV/r (assumed, DHHS-listed dual regimen)") %>%
    mutate(naive_frac = multicore_other_naive, art_class = "Multicore") %>%
    dplyr::select(art_class, regimen, naive_frac, floor_price, median_price, ceiling_price)

regimen_prices_with_assumption <- bind_rows(regimen_prices, assumed_multicore_other)

art_cost_naive_with_assumption <- regimen_prices_with_assumption %>%
    summarise(
        coverage_pct = sum(naive_frac) * 100,
        low    = sum(floor_price   * naive_frac) / sum(naive_frac) * 12,
        median = sum(median_price  * naive_frac) / sum(naive_frac) * 12,
        high   = sum(ceiling_price * naive_frac) / sum(naive_frac) * 12
    )

art_cost_naive_with_assumption

## ---- remaining unspecified-backbone rows, filled with DHHS Recommended ----
## ---- Initial Regimens (clinicalinfo.hiv.gov), not Table 1 itself ----
## Dolutegravir-other (INSTI 96% x 11% x (1-0.32-0.10) = 6.12%): guidelines list
## "DTG plus (TAF or TDF) plus (FTC or 3TC)" as AI. No single-tablet DTG/TAF/FTC
## product exists, so this is priced as DTG + Descovy (TAF/FTC).
## Raltegravir alone (INSTI 96% x 1% = 0.96%): guidelines rate RAL + TDF/FTC as
## BI (vs. BII for RAL + TAF/FTC) -- TDF/FTC has the stronger evidence rating
## for RAL specifically, so that's used here.
## Darunavir-other (PI 1% x 100% x (1-0.75) = 0.25%): guidelines list
## "(DRV/c or DRV/r) plus (TAF or TDF) plus (FTC or 3TC)" as AI, ahead of
## DRV/r plus ABC/3TC (BII) -- priced as DRV/r + Descovy (TAF/FTC).
dolutegravir_other_naive <- insti_naive * 0.11 * (1 - 0.32 - 0.10)
raltegravir_alone_naive  <- insti_naive * 0.01
darunavir_other_naive    <- pi_naive * 1.00 * (1 - 0.75)

dtg_taf_ftc   <- combined_price(c("DTG"), taf_ftc_price)
ral_tdf_ftc   <- combined_price(c("RAL"), tdf_ftc_price)
drv_taf_ftc   <- combined_price(c("DRV", "RTV"), taf_ftc_price)

gap_regimens <- bind_rows(
    dtg_taf_ftc %>% mutate(regimen = "DTG + TAF/FTC (assumed, DHHS AI)",   naive_frac = dolutegravir_other_naive, art_class = "INSTI-based"),
    ral_tdf_ftc %>% mutate(regimen = "RAL + TDF/FTC (assumed, DHHS BI)",   naive_frac = raltegravir_alone_naive,  art_class = "INSTI-based"),
    drv_taf_ftc %>% mutate(regimen = "DRV/r + TAF/FTC (assumed, DHHS AI)", naive_frac = darunavir_other_naive,    art_class = "PI-based")
) %>%
    rowwise() %>%
    mutate(median_price = median(c(floor_price, ceiling_price))) %>%
    ungroup() %>%
    dplyr::select(art_class, regimen, naive_frac, floor_price, median_price, ceiling_price)

gap_regimens

## ---- fully-filled coverage: Table 1 named regimens + all four assumed rows ----
regimen_prices_full <- bind_rows(regimen_prices, assumed_multicore_other, gap_regimens)

regimen_prices_full

art_cost_naive_full <- regimen_prices_full %>%
    summarise(
        coverage_pct = sum(naive_frac) * 100,
        low    = sum(floor_price   * naive_frac) / sum(naive_frac) * 12,
        median = sum(median_price  * naive_frac) / sum(naive_frac) * 12,
        high   = sum(ceiling_price * naive_frac) / sum(naive_frac) * 12
    )

art_cost_naive_full

## ---- Supplementary table: FSS monthly range and % naive PWH, by ART class ----
## Range = min floor price to max ceiling price across the named/assumed
## regimens within each class (same definition as the original script's
## table_s3_naive: fss_low = min(floor_price), fss_high = max(ceiling_price)).
## This spans both the cheapest-vs-priciest regimen AND floor-vs-ceiling
## pricing within a regimen, so it will run wider than an FSS-only spread.
## % Naive PWH = the class-level naive-population share from Table 1
## (INSTI 96%, PI 1%, NNRTI 1%, Multicore 2%); these do not depend on how many
## regimens are modeled within the class.
class_naive_pct <- tribble(
    ~art_class,     ~pct_naive,
    "INSTI-based",  insti_naive * 100,
    "PI-based",     pi_naive * 100,
    "NNRTI-based",  nnrti_naive * 100,
    "Multicore",    multicore_naive * 100
)

class_summary <- regimen_prices_full %>%
    group_by(art_class) %>%
    summarise(
        fss_low    = min(floor_price, na.rm = TRUE),
        fss_high   = max(ceiling_price, na.rm = TRUE),
        fss_median = median(c(floor_price, ceiling_price), na.rm = TRUE),
        .groups = "drop"
    ) %>%
    left_join(class_naive_pct, by = "art_class")

supp_table <- class_summary %>%
    transmute(
        `ART class` = art_class,
        `FSS Monthly range (2026 USD)` = paste0("$", round(fss_low), "-$", round(fss_high)),
        `% Naive PWH on each regimen` = paste0(round(pct_naive), "%")
    )

supp_table

## diagnostic: which regimen is driving each class's floor/ceiling
regimen_prices_full %>%
    arrange(art_class, ceiling_price) %>%
    dplyr::select(art_class, regimen, naive_frac, floor_price, ceiling_price)

## ---- class-weighted annual ART cost (analog of the original art_cost_scenarios) ----
## Weights each class's fss_low/fss_median/fss_high by the class-level naive %
## (INSTI 96%, PI 1%, NNRTI 1%, Multicore 2%) -- coarser than the regimen-level
## naive_frac weighting in art_cost_naive_full, since every regimen within a
## class is treated as equally likely rather than weighted by its own share.
art_cost_class_weighted <- class_summary %>%
    summarise(
        low    = sum(fss_low    * pct_naive / 100) * 12,
        median = sum(fss_median * pct_naive / 100) * 12,
        high   = sum(fss_high   * pct_naive / 100) * 12
    )

art_cost_class_weighted

## ================================================================
## OVERALL POPULATION VERSION -- Table 1's "Overall (N = 13,434)" column
## instead of the ARV-naive subset. This is current regimen distribution
## across ALL PWH in 2019-2020, so it captures people on multi-core/
## post-failure regimens, not just what people are started on.
## Reuses the same component prices, get_fdc_price(), combo_price(), and
## combined_price() built above -- only the weights change.
## ================================================================

insti_overall      <- 0.742
pi_overall         <- 0.061
nnrti_overall      <- 0.085
multicore_overall  <- 0.112

table1_regimens_overall <- tribble(
    ~art_class,      ~regimen,                        ~type,   ~keyword,    ~overall_frac,
    "INSTI-based",   "BIC/TAF/FTC (Biktarvy)",        "fdc",   "BIKTARVY",  insti_overall * 0.48,
    "INSTI-based",   "DTG/ABC/3TC (Triumeq)",         "fdc",   "TRIUMEQ",   insti_overall * 0.33 * 0.49,
    "INSTI-based",   "DTG/3TC (Dovato)",              "fdc",   "DOVATO",    insti_overall * 0.33 * 0.04,
    "INSTI-based",   "EVG/c/TAF/FTC (Genvoya)",       "fdc",   "GENVOYA",   insti_overall * 0.17,
    "INSTI-based",   "EVG/c/TDF/FTC (Stribild)",      "fdc",   "STRIBILD",  insti_overall * 0.01,
    "PI-based",      "DRV/c/TAF/FTC (Symtuza)",       "fdc",   "SYMTUZA",   pi_overall * 0.88 * 0.48,
    "NNRTI-based",   "RPV/TAF/FTC (Odefsey)",         "fdc",   "ODEFSEY",   nnrti_overall * 0.67,
    "NNRTI-based",   "RPV/TDF/FTC (Complera)",        "fdc",   "COMPLERA",  nnrti_overall * 0.06,
    "NNRTI-based",   "EFV/TDF/FTC (Atripla)",         "fdc",   "ATRIPLA",   nnrti_overall * 0.15,
    "Multicore",     "DTG/RPV (Juluca)",              "fdc",   "JULUCA",    multicore_overall * 0.96 * 0.15,
    "Multicore",     "DTG + DRV/r (multicore)",       "combo", NA,          multicore_overall * 0.96 * 0.40
)

regimen_prices_overall <- table1_regimens_overall %>%
    rowwise() %>%
    mutate(
        prices = list(
            if (type == "fdc") get_fdc_price(keyword) else combo_price(c("DTG", "DRV", "RTV"))
        )
    ) %>%
    unnest(prices) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(median_price = median(c(floor_price, ceiling_price))) %>%
    ungroup() %>%
    dplyr::select(art_class, regimen, overall_frac, floor_price, median_price, ceiling_price)

## ---- multicore remainder (INSTI + other core drug, not DTG+DRV or Juluca) ----
## same DHHS-listed dual regimen (RAL + DRV/r) used in the naive version
multicore_other_overall_naive <- multicore_overall * 0.96 * (1 - 0.40 - 0.15)

assumed_multicore_other_overall <- assumed_candidates %>%
    filter(regimen == "RAL + DRV/r (assumed, DHHS-listed dual regimen)") %>%
    mutate(overall_frac = multicore_other_overall_naive, art_class = "Multicore") %>%
    dplyr::select(art_class, regimen, overall_frac, floor_price, median_price, ceiling_price)

## ---- remaining unspecified-backbone rows, overall column ----
## Dolutegravir-other, Raltegravir-alone, Darunavir-other: same DHHS-backed
## backbone picks as the naive version (dtg_taf_ftc, ral_tdf_ftc, drv_taf_ftc
## already computed above), just re-weighted by overall prevalence.
## Two NEW gaps open up here that were ~0% in the naive column:
##   Atazanavir alone (12% of PI overall): guidelines rate (ATV/c or ATV/r)
##   plus (TAF or TDF) plus (FTC or 3TC) as BI -- priced as ATV/r + Descovy.
##   Doravirine alone (2% of NNRTI overall): guidelines rate DOR/TDF/3TC as
##   BI and DOR plus TAF/FTC as BIII -- TDF-based Delstrigo is on the
##   HIV Assist exclusion list, so DOR + Descovy (TAF/FTC) is used despite
##   the lower BIII rating, consistent with the TAF-preference already
##   applied to the other backbone-filled rows.
dolutegravir_other_overall <- insti_overall * 0.33 * (1 - 0.49 - 0.04)
raltegravir_alone_overall  <- insti_overall * 0.02
darunavir_other_overall    <- pi_overall * 0.88 * (1 - 0.48)
atazanavir_alone_overall   <- pi_overall * 0.12
doravirine_alone_overall   <- nnrti_overall * 0.02

atz_taf_ftc <- combined_price(c("ATZ", "RTV"), taf_ftc_price)
dor_taf_ftc <- combined_price(c("DOR"), taf_ftc_price)

gap_regimens_overall <- bind_rows(
    dtg_taf_ftc %>% mutate(regimen = "DTG + TAF/FTC (assumed, DHHS AI)",     overall_frac = dolutegravir_other_overall, art_class = "INSTI-based"),
    ral_tdf_ftc %>% mutate(regimen = "RAL + TDF/FTC (assumed, DHHS BI)",     overall_frac = raltegravir_alone_overall,  art_class = "INSTI-based"),
    drv_taf_ftc %>% mutate(regimen = "DRV/r + TAF/FTC (assumed, DHHS AI)",   overall_frac = darunavir_other_overall,    art_class = "PI-based"),
    atz_taf_ftc %>% mutate(regimen = "ATV/r + TAF/FTC (assumed, DHHS BI)",   overall_frac = atazanavir_alone_overall,   art_class = "PI-based"),
    dor_taf_ftc %>% mutate(regimen = "DOR + TAF/FTC (assumed, DHHS BIII)",   overall_frac = doravirine_alone_overall,   art_class = "NNRTI-based")
) %>%
    rowwise() %>%
    mutate(median_price = median(c(floor_price, ceiling_price))) %>%
    ungroup() %>%
    dplyr::select(art_class, regimen, overall_frac, floor_price, median_price, ceiling_price)

gap_regimens_overall

## ---- fully-filled overall-population coverage ----
regimen_prices_overall_full <- bind_rows(regimen_prices_overall, assumed_multicore_other_overall, gap_regimens_overall)

regimen_prices_overall_full

## regimen-level weighted annual cost, overall population
art_cost_overall_full <- regimen_prices_overall_full %>%
    summarise(
        coverage_pct = sum(overall_frac) * 100,
        low    = sum(floor_price   * overall_frac) / sum(overall_frac) * 12,
        median = sum(median_price  * overall_frac) / sum(overall_frac) * 12,
        high   = sum(ceiling_price * overall_frac) / sum(overall_frac) * 12
    )

art_cost_overall_full

## ---- class-level supplementary table, overall population ----
class_naive_pct_overall <- tribble(
    ~art_class,     ~pct_overall,
    "INSTI-based",  insti_overall * 100,
    "PI-based",     pi_overall * 100,
    "NNRTI-based",  nnrti_overall * 100,
    "Multicore",    multicore_overall * 100
)

class_summary_overall <- regimen_prices_overall_full %>%
    group_by(art_class) %>%
    summarise(
        fss_low    = min(floor_price, na.rm = TRUE),
        fss_high   = max(ceiling_price, na.rm = TRUE),
        fss_median = median(c(floor_price, ceiling_price), na.rm = TRUE),
        .groups = "drop"
    ) %>%
    left_join(class_naive_pct_overall, by = "art_class")

supp_table_overall <- class_summary_overall %>%
    transmute(
        `ART class` = art_class,
        `FSS Monthly range (2026 USD)` = paste0("$", round(fss_low), "-$", round(fss_high)),
        `% of Overall PWH on each regimen` = paste0(round(pct_overall), "%")
    )

supp_table_overall

## class-weighted annual cost, overall population (analog of art_cost_class_weighted)
art_cost_class_weighted_overall <- class_summary_overall %>%
    summarise(
        low    = sum(fss_low    * pct_overall / 100) * 12,
        median = sum(fss_median * pct_overall / 100) * 12,
        high   = sum(fss_high   * pct_overall / 100) * 12
    )

art_cost_class_weighted_overall
