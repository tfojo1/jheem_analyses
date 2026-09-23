source("data_processing/hiv.surveillance.manager/manager_value_delta.R")

old.nsduh <- matrix(c(0.1, 0.2), nrow = 2,
                    dimnames = list(year = c("2020", "2021"),
                                    location = "A"))
new.nsduh <- cbind(old.nsduh, B = c(0.3, NaN))
names(dimnames(new.nsduh)) <- c("year", "location")
old.pop <- matrix(c(10, 11), nrow = 2,
                  dimnames = list(year = c("2020", "2021"),
                                  location = "A"))
new.pop <- old.pop
new.pop["2021", "A"] <- 12

old <- list(nsduh = old.nsduh, population = old.pop)
new <- list(nsduh = new.nsduh, population = new.pop)
report <- compare.manager.data.values(old, new)

stopifnot(report$status == "diagnostic_only",
          report$arrays_compared == 2L,
          report$arrays_with_differences == 2L,
          report$changed_overlap_cells == 1L,
          identical(report$changes[[1]]$added_dimension_values$location, "B"),
          report$changes[[1]]$changed_overlap_cells == 0L,
          identical(report$changes[[2]]$changed_dimension_values$year, "2021"),
          identical(report$changes[[2]]$changed_dimension_values$location, "A"))

new.pop["2021", "A"] <- NA_real_
report <- compare.manager.data.values(old, list(nsduh = new.nsduh,
                                                population = new.pop))
stopifnot(report$changed_overlap_cells == 1L)

reordered.nsduh <- new.nsduh[c("2021", "2020"), c("B", "A")]
report <- compare.manager.data.values(list(nsduh = old.nsduh),
                                      list(nsduh = reordered.nsduh))
stopifnot(report$changed_overlap_cells == 0L,
          identical(report$changes[[1]]$added_dimension_values$location, "B"))

cat("Manager value-delta diagnostics passed\n")
