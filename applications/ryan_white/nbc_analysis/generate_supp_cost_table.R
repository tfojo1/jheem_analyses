# Making supplemental table

source("../jheem_analyses/applications/ryan_white/read_rw_costs.R")
RW.STATES = c('AL', 'AR', 'AZ', 'CA', 'CO',"DC",
              'FL', 'GA', 'IL', 'IN', 'KY',
              'LA', 'MA', 'MD', 'MI', 'MN',
              'MO', 'MS', 'NC', 'NJ', 'NV',
              'NY', 'OH', 'OK', 'PA', 'SC',
              'TN', 'TX', 'VA', 'WA', 'WI')

cost_table <- as.data.frame(state.rw.costs) %>%
    rownames_to_column("state") %>%
    filter(state %in% RW.STATES) %>%
    mutate(State = locations::get.location.name(state)) %>%
    arrange(State) %>%
    mutate(part.c = part.c.eis + part.c.capacity,
           minority = part.a.minority + part.b.minority,
           total.non.adap = total - part.b.adap,
           total.cut = part.c + part.d + minority + ehe,
           percent.cut = 100 * total.cut / total.non.adap) %>%
    select(State,
           part.a,
           part.b.non.adap,
           part.c,
           part.d,
           ehe,
           minority,
           total.non.adap,
           total.cut,
           percent.cut) %>%
    rename(`Part A` = part.a,
           `Part B non-ADAP` = part.b.non.adap,
           `Part C` = part.c,
           `Part D` = part.d,
           `EHE` = ehe,
           `Minority AIDS Initiative from parts A and B` = minority,
           `Total non-ADAP` = total.non.adap,
           `Total Potential Cut` = total.cut,
           `Percent Potential Cut` = percent.cut)
write.csv(cost_table,
          "../../results/ryan_white/nbc/cost_table.csv",
          row.names = FALSE)

# After generating CSV, open in Excel and convert most cols to currency with
# symbol set to "none" in settings. 0 decimals on all columns.