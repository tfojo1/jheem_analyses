source('../jheem_analyses/applications/EHE/ehe_specification.R')

load('prelim_results/full.with.covid2_simset_2025-01-29_C.12580.Rdata')
simset = simset$burn(keep = 100)
save(simset,file="~/OneDrive - Johns Hopkins/simsets_for_lauren/simset_2025_01-31_C.12580.Rdata")

load('prelim_results/full.with.covid2_simset_2025-01-30_C.35620.Rdata')
simset = simset$burn(keep = 100)
save(simset,file="~/OneDrive - Johns Hopkins/simsets_for_lauren/simset_2025_01-31_C.35620.Rdata")

load('prelim_results/full.with.covid2_simset_2025-01-31_C.16980.Rdata')
simset = simset$burn(keep = 100)
save(simset,file="~/OneDrive - Johns Hopkins/simsets_for_lauren/simset_2025_01-31_C.16980.Rdata")

load('prelim_results/full.with.covid2_simset_2025-01-29_C.26420.Rdata')
simset = simset$burn(keep = 100)
save(simset,file="~/OneDrive - Johns Hopkins/simsets_for_lauren/simset_2025_01-31_C.26420.Rdata")

load('prelim_results/full.with.covid2_simset_2025-01-29_C.12060.Rdata')
simset = simset$burn(keep = 100)
save(simset,file="~/OneDrive - Johns Hopkins/simsets_for_lauren/simset_2025_01-31_C.12060.Rdata")


