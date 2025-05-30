# QoL Data Manager - Example Usage
# Simple examples of pulling data from the QoL/SDOH data manager

library(jheem2)
library(dplyr)

# NAS Detection
NAS.PATHS <- c("Q:", "/Volumes/jheem$", "/mnt/jheem_nas_share")
ROOT.DIR <- NULL

for(path in NAS.PATHS) {
    if(dir.exists(path)) {
        ROOT.DIR <- path
        break
    }
}

# Load the data manager (NAS first, then local)
if(!is.null(ROOT.DIR) && file.exists(file.path(ROOT.DIR, "data_managers", "pwh.qol.sdoh.manager.rdata"))) {
    qol_manager <- load.data.manager(file=file.path(ROOT.DIR, "data_managers", "pwh.qol.sdoh.manager.rdata"))
} else {
    qol_manager <- load.data.manager(file="cached/pwh.qol.sdoh.manager.rdata")
}

# =============================================================================
# EXAMPLE PULLS
# =============================================================================

# Example 1: US HIV stigma by year
us_stigma <- qol_manager$pull(
    outcome = "hiv.stigma.pwh",
    dimension.values = list(
        location = "US", 
        age = "13+ years", 
        race = "all races",
        sex = "all",
        risk = "all transmission"
    ),
    keep.dimensions = "year"
)

print("US HIV Stigma by year:")
print(us_stigma)

# Example 2: All states for 2022
states_2022 <- qol_manager$pull(
    outcome = "hiv.stigma.pwh",
    dimension.values = list(
        year = "2022",
        age = "13+ years", 
        race = "all races",
        sex = "all",
        risk = "all transmission"
    ),
    keep.dimensions = "location"
)

print("HIV Stigma by state (2022):")
print(head(states_2022, 10))

# Example 3: Age-stratified data for US, 2022
us_by_age <- qol_manager$pull(
    outcome = "hiv.stigma.pwh",
    dimension.values = list(
        location = "US",
        year = "2022",
        race = "all races",
        sex = "all", 
        risk = "all transmission"
    ),
    keep.dimensions = "age"
)

print("US HIV Stigma by age group (2022):")
print(us_by_age)

# Example 4: All QoL outcomes for US 2022
outcomes <- c("unstable.housing.pwh", "hiv.stigma.pwh", "good.health.pwh", 
              "unmet.mental.health.pwh", "food.insecurity.pwh", "unemployment.pwh")

us_2022_summary <- data.frame(
    outcome = character(),
    value = numeric(),
    stringsAsFactors = FALSE
)

for(outcome in outcomes) {
    result <- qol_manager$pull(
        outcome = outcome,
        dimension.values = list(
            location = "US",
            year = "2022", 
            age = "13+ years", 
            race = "all races",
            sex = "all",
            risk = "all transmission"
        ),
        keep.dimensions = "year"
    )
    
    if(!is.null(result)) {
        value_2022 <- as.numeric(result["2022", ])
        us_2022_summary <- rbind(us_2022_summary, 
                                data.frame(outcome = outcome, value = value_2022))
    }
}

print("All QoL outcomes for US 2022:")
print(us_2022_summary)

# Example 5: Year x Location matrix for one outcome
food_insecurity_matrix <- qol_manager$pull(
    outcome = "food.insecurity.pwh",
    dimension.values = list(
        age = "13+ years", 
        race = "all races",
        sex = "all",
        risk = "all transmission"
    ),
    keep.dimensions = c("year", "location")
)

print("Food Insecurity - Year x Location matrix:")
print(food_insecurity_matrix[, 1:5, 1])  # First 5 locations
