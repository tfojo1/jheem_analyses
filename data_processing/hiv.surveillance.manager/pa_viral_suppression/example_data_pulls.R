#!/usr/bin/env Rscript
# Example Data Pulls from Surveillance Manager - Direct Access Method
# Using $ syntax instead of pull() function

library(jheem2)

cat("================================================================================\n")
cat("                    SURVEILLANCE MANAGER DATA ACCESS EXAMPLES                  \n")
cat("================================================================================\n\n")

# Load the manager
manager <- load.data.manager("cached/surveillance.manager.pa_5age.rdata")

cat("PART 1: PENNSYLVANIA DATA (NEWLY INTEGRATED)\n")
cat("=" , rep("", 78), "\n\n")

# ============================================================================
# 1. STATE-LEVEL TOTALS
# ============================================================================
cat("1. PA STATE-LEVEL TOTALS (no stratification)\n")
cat("-" , rep("", 40), "\n")

total_data <- manager$data$suppression$estimate$cdc.hiv$cdc[["year__location"]]

# PA 2022 and 2023
pa_2022_total <- total_data["2022", "PA"]
pa_2023_total <- total_data["2023", "PA"]

cat(sprintf("PA 2022 Total: %.3f (%.1f%%)\n", pa_2022_total, pa_2022_total * 100))
cat(sprintf("PA 2023 Total: %.3f (%.1f%%)\n", pa_2023_total, pa_2023_total * 100))
cat("\n")

# ============================================================================
# 2. COUNTY-LEVEL TOTALS
# ============================================================================
cat("2. PA COUNTY-LEVEL TOTALS\n")
cat("-" , rep("", 40), "\n")

# Philadelphia County (42101)
phila_2022 <- total_data["2022", "42101"]
phila_2023 <- total_data["2023", "42101"]
cat(sprintf("Philadelphia County (42101):\n"))
cat(sprintf("  2022: %.3f (%.1f%%)\n", phila_2022, phila_2022 * 100))
cat(sprintf("  2023: %.3f (%.1f%%)\n", phila_2023, phila_2023 * 100))

# Allegheny County (42003) - Pittsburgh
allegheny_2022 <- total_data["2022", "42003"]
allegheny_2023 <- total_data["2023", "42003"]
cat(sprintf("Allegheny County (42003):\n"))
cat(sprintf("  2022: %.3f (%.1f%%)\n", allegheny_2022, allegheny_2022 * 100))
cat(sprintf("  2023: %.3f (%.1f%%)\n", allegheny_2023, allegheny_2023 * 100))

# Small county example - Adams (42001)
adams_2022 <- total_data["2022", "42001"]
adams_2023 <- total_data["2023", "42001"]
cat(sprintf("Adams County (42001):\n"))
cat(sprintf("  2022: %.3f (%.1f%%)\n", adams_2022, adams_2022 * 100))
cat(sprintf("  2023: %.3f (%.1f%%)\n", adams_2023, adams_2023 * 100))
cat("\n")

# ============================================================================
# 3. AGE-STRATIFIED DATA (1-way)
# ============================================================================
cat("3. PA AGE-STRATIFIED DATA\n")
cat("-" , rep("", 40), "\n")

age_data <- manager$data$suppression$estimate$cdc.hiv$cdc[["year__location__age"]]

# PA 2022 by age
cat("PA 2022 by Age Group:\n")
age_groups <- dimnames(age_data)$age
for (age in age_groups) {
  val <- age_data["2022", "PA", age]
  if (!is.na(val) && !is.nan(val)) {
    cat(sprintf("  %s: %.3f (%.1f%%)\n", age, val, val * 100))
  }
}

# County example - Philadelphia 2022 by age
cat("\nPhiladelphia County 2022 by Age:\n")
for (age in age_groups) {
  val <- age_data["2022", "42101", age]
  if (!is.na(val) && !is.nan(val)) {
    cat(sprintf("  %s: %.3f (%.1f%%)\n", age, val, val * 100))
  }
}
cat("\n")

# ============================================================================
# 4. SEX-STRATIFIED DATA (1-way)
# ============================================================================
cat("4. PA SEX-STRATIFIED DATA\n")
cat("-" , rep("", 40), "\n")

sex_data <- manager$data$suppression$estimate$cdc.hiv$cdc[["year__location__sex"]]

# PA 2022 by sex
cat("PA 2022 by Sex:\n")
sex_values <- dimnames(sex_data)$sex
for (sex in sex_values) {
  val <- sex_data["2022", "PA", sex]
  if (!is.na(val) && !is.nan(val)) {
    cat(sprintf("  %s: %.3f (%.1f%%)\n", sex, val, val * 100))
  }
}
cat("\n")

# ============================================================================
# 5. RACE-STRATIFIED DATA (1-way)
# ============================================================================
cat("5. PA RACE-STRATIFIED DATA\n")
cat("-" , rep("", 40), "\n")

race_data <- manager$data$suppression$estimate$cdc.hiv$cdc[["year__location__race"]]

# PA 2022 by race
cat("PA 2022 by Race/Ethnicity:\n")
race_values <- dimnames(race_data)$race
for (race in race_values) {
  val <- race_data["2022", "PA", race]
  if (!is.na(val) && !is.nan(val)) {
    # Truncate long race names for display
    display_race <- if(nchar(race) > 30) paste0(substr(race, 1, 27), "...") else race
    cat(sprintf("  %-30s: %.3f (%.1f%%)\n", display_race, val, val * 100))
  }
}
cat("\n")

# ============================================================================
# 6. RISK-STRATIFIED DATA (1-way)
# ============================================================================
cat("6. PA RISK-STRATIFIED DATA\n")
cat("-" , rep("", 40), "\n")

risk_data <- manager$data$suppression$estimate$cdc.hiv$cdc[["year__location__risk"]]

# PA 2022 by transmission category
cat("PA 2022 by Transmission Category:\n")
risk_values <- dimnames(risk_data)$risk
for (risk in risk_values) {
  val <- risk_data["2022", "PA", risk]
  if (!is.na(val) && !is.nan(val)) {
    cat(sprintf("  %-15s: %.3f (%.1f%%)\n", risk, val, val * 100))
  }
}
cat("\n")

# ============================================================================
# 7. TWO-WAY STRATIFICATIONS
# ============================================================================
cat("7. PA TWO-WAY STRATIFIED DATA\n")
cat("-" , rep("", 40), "\n")

# Age × Sex
age_sex_data <- manager$data$suppression$estimate$cdc.hiv$cdc[["year__location__age__sex"]]
cat("PA 2022 - Age × Sex (sample):\n")
for (age in c("13-24 years", "55+ years")) {
  for (sex in c("male", "female")) {
    val <- age_sex_data["2022", "PA", age, sex]
    if (!is.na(val) && !is.nan(val)) {
      cat(sprintf("  %s, %s: %.3f (%.1f%%)\n", age, sex, val, val * 100))
    }
  }
}

cat("\n")

# Race × Sex
race_sex_data <- manager$data$suppression$estimate$cdc.hiv$cdc[["year__location__race__sex"]]
cat("PA 2022 - Race × Sex (sample):\n")
for (race in c("white", "black/african american")) {
  for (sex in c("male", "female")) {
    val <- race_sex_data["2022", "PA", race, sex]
    if (!is.na(val) && !is.nan(val)) {
      cat(sprintf("  %-22s, %-6s: %.3f (%.1f%%)\n", race, sex, val, val * 100))
    }
  }
}

cat("\n================================================================================\n\n")

cat("PART 2: COMPARISON WITH OTHER STATES (NOT MODIFIED)\n")
cat("=" , rep("", 78), "\n\n")

# ============================================================================
# 8. OTHER STATE COMPARISONS
# ============================================================================
cat("8. OTHER STATES - TOTAL SUPPRESSION\n")
cat("-" , rep("", 40), "\n")

# Check some neighboring states for comparison
comparison_states <- c("NY" = "New York", 
                      "NJ" = "New Jersey",
                      "OH" = "Ohio",
                      "MD" = "Maryland",
                      "CA" = "California",
                      "TX" = "Texas")

cat("2021 Total Suppression (latest year for most states):\n")
for (state_code in names(comparison_states)) {
  val_2021 <- total_data["2021", state_code]
  if (!is.na(val_2021) && !is.nan(val_2021)) {
    cat(sprintf("  %-15s (%s): %.3f (%.1f%%)\n", 
                comparison_states[state_code], state_code, val_2021, val_2021 * 100))
  }
}

cat("\n")

# ============================================================================
# 9. NEW YORK STATE AGE-STRATIFIED (for comparison with PA)
# ============================================================================
cat("9. NEW YORK AGE-STRATIFIED DATA (2021)\n")
cat("-" , rep("", 40), "\n")

cat("NY 2021 by Age Group:\n")
for (age in age_groups) {
  val <- age_data["2021", "NY", age]
  if (!is.na(val) && !is.nan(val)) {
    cat(sprintf("  %s: %.3f (%.1f%%)\n", age, val, val * 100))
  }
}

cat("\n")

# ============================================================================
# 10. CALIFORNIA COUNTY EXAMPLE
# ============================================================================
cat("10. CALIFORNIA COUNTIES (2021)\n")
cat("-" , rep("", 40), "\n")

# Los Angeles County (06037)
la_2021 <- total_data["2021", "06037"]
if (!is.na(la_2021) && !is.nan(la_2021)) {
  cat(sprintf("Los Angeles County (06037): %.3f (%.1f%%)\n", la_2021, la_2021 * 100))
}

# San Francisco County (06075)
sf_2021 <- total_data["2021", "06075"]
if (!is.na(sf_2021) && !is.nan(sf_2021)) {
  cat(sprintf("San Francisco County (06075): %.3f (%.1f%%)\n", sf_2021, sf_2021 * 100))
}

cat("\n================================================================================\n\n")

cat("PART 3: DATA VERIFICATION CHECKLIST\n")
cat("=" , rep("", 78), "\n\n")

cat("Values to verify against Atlas Plus UI:\n\n")

cat("PENNSYLVANIA 2022:\n")
cat("  □ State total: 63.1%\n")
cat("  □ Philadelphia County: Check value shown above\n")
cat("  □ Age 13-24: 63.6%\n")
cat("  □ Age 55+: 65.2% (combined from 55-64 and 65+)\n")
cat("  □ Male: Check value shown above\n")
cat("  □ Female: Check value shown above\n")
cat("  □ MSM transmission: Check value shown above\n")

cat("\nPENNSYLVANIA 2023:\n")
cat("  □ State total: 64.7%\n")
cat("  □ Philadelphia County: Check value shown above\n")

cat("\nCOMPARISON STATES (2021):\n")
cat("  □ New York total: Check value shown above\n")
cat("  □ California (Los Angeles County): Check value shown above\n")

cat("\n================================================================================\n")
cat("                         END OF EXAMPLES                                       \n")
cat("================================================================================\n")
