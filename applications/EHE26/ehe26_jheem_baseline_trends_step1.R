# EHE26 STEP 1: load each state's JHEEM simset ONCE and save the raw model numbers
#
# Purpose
#   Loading a simset is slow. This step does it once,
#   saves everything step 2 needs, and closes the simset. Step 2
#   (ehe26_jheem_baseline_trends_step2.R) then builds all measures, groups and figures from these
#   saved numbers, so you can change measures without ever reloading simulations.
#
# Which simsets: SIMSET.FILE in ehe26_jheem_settings.R, now 'baseline'.
#   1. 'baseline' files have every year from 1970 to the end of the projection, so we get the
#      years before 2025 to compare with the observed data. 'noint' files start in 2025.
#   2. They are ~1.5 GB each (noint ~0.4 GB), so loading takes much longer and uses more memory.
#      That is why N.CORES is lower than for noint.
#   3. Extracts go to <OUT.DIR>/jheem_baseline_raw/. The earlier noint extracts in
#      jheem_noint_raw/ are not touched.
#
# What it saves: one file per state in RAW.DIR (see ehe26_jheem_settings.R)
#   <RAW.DIR>/<state>.rds = list(
#       info     = state, simset file, years, number of simulations, date
#       outcomes = one entry per model outcome (RAW.OUTCOMES below):
#                    counts:            value       = array  year x ... x sim
#                    proportions/rates: numerator, denominator = arrays  year x ... x sim
#       problems = any outcome that could not be read, with the error message)
#
# Why numerators and denominators
#   Step 2 makes groups by adding up cells. For a proportion you must add numerators and
#   denominators separately, then divide.
#   Example, suppression for MSM: (suppressed MSM, all ages and races) / (diagnosed MSM, all ages and races).
#   Averaging the cell proportions instead would give the wrong answer.
#
# Breakdowns saved: year x age x race x sex (x sim). Not risk: MSM is a sex category in the model
#   ('msm', 'heterosexual_male', 'female'), and we do not split by injection drug use.
#   Awareness keeps only year x age, tests per population only year (that is all the model keeps).
#   Size: about 50-100 MB per state with 1000 simulations.
#
# How to run (from the jheem_analyses/ folder)
#   Terminal (states in parallel, recommended):
#     nohup Rscript applications/EHE26/ehe26_jheem_baseline_trends_step1.R > jheem_extract.log 2>&1 &
#     tail -f jheem_extract.log
#   RStudio: set N.CORES = 1 first (forking does not work reliably inside RStudio).
#
# Running it again by mistake is safe
#   1. Section 2 looks in RAW.DIR first. A state whose <state>.rds is already there is skipped.
#   2. If every state is already saved, the script stops there: it does not load jheem2 or any simset.
#   3. To redo states on purpose, set FORCE.EXTRACT = T (and STATES.TO.RUN to just those states).
#   4. Each file is written under a temporary name and renamed only when complete,
#      so a run that crashes never leaves a half-written <state>.rds that looks finished.
#
# Faster while developing
#   1. STATES.TO.RUN = c('AL', 'MD', 'TX')  extract a few states first
#   2. N.SIM.KEEP = 200                     keep fewer simulations (set back to NULL for the final run,
#                                           with FORCE.EXTRACT = T)

source('../jheem_analyses/applications/EHE26/ehe26_jheem_settings.R')

# 1. SETTINGS FOR THIS STEP ----
N.CORES = 4                      # states loaded at the same time. Baseline files are ~1.5 GB each and
                                 # take several times that in memory; 4 keeps well under 96 GB.
                                 # Watch memory (Activity Monitor) on the first states; raise only if there is room.
FORCE.EXTRACT = F                # T = redo states that were already saved (overwrites their files)
STATES.TO.RUN = MODEL.STATES     # e.g. 'AL' to test one state
N.SIM.KEEP = NULL                # NULL = all simulations; e.g. 200 = thin to 200 (faster, for development)

# 2. SKIP STATES THAT ARE ALREADY SAVED ----
if (!dir.exists(RAW.DIR))
    dir.create(RAW.DIR, recursive = T)
raw.file = function(state) file.path(RAW.DIR, paste0(state, '.rds'))

already.saved = STATES.TO.RUN[file.exists(raw.file(STATES.TO.RUN))]
if (length(already.saved) > 0) {
    if (FORCE.EXTRACT) {
        print(paste0("FORCE.EXTRACT = T: OVERWRITING ", length(already.saved), " saved states: ",
                     paste(already.saved, collapse = ', ')))
    } else {
        print(paste0("Skipping ", length(already.saved), " states already saved in ", RAW.DIR, ": ",
                     paste(already.saved, collapse = ', ')))
        STATES.TO.RUN = setdiff(STATES.TO.RUN, already.saved)
    }
}
print(paste0(length(STATES.TO.RUN), " states to extract into ", RAW.DIR,
             if (length(STATES.TO.RUN) > 0) paste0(": ", paste(STATES.TO.RUN, collapse = ', ')) else ''))

# jheem2 and the EHE spec are loaded only if there is something to extract
if (length(STATES.TO.RUN) > 0)
    source('../jheem_analyses/applications/EHE/ehe_specification.R')

# 3. MODEL OUTCOMES TO SAVE ----
# Definitions: see the MODEL OUTCOME DICTIONARY at the top of ehe26_jheem_baseline_trends.R
#   type 'count'      -> saved as value
#   type 'proportion' -> saved as numerator and denominator
#   type 'rate'       -> saved as numerator and denominator (tests / population)
#   keep = breakdowns kept (year is always first, sim is added by jheem2)
AGE.RACE.SEX = c('year', 'age', 'race', 'sex')
RAW.OUTCOMES = list(
    prep.uptake                    = list(type = 'count',      keep = AGE.RACE.SEX),
    prep.indications               = list(type = 'count',      keep = AGE.RACE.SEX),
    total.hiv.tests                = list(type = 'count',      keep = AGE.RACE.SEX),
    population                     = list(type = 'count',      keep = AGE.RACE.SEX),
    suppression                    = list(type = 'proportion', keep = AGE.RACE.SEX),
    testing                        = list(type = 'proportion', keep = AGE.RACE.SEX),
    general.population.testing     = list(type = 'rate',       keep = AGE.RACE.SEX),
    awareness                      = list(type = 'proportion', keep = c('year', 'age')),
    total.hiv.tests.per.population = list(type = 'proportion', keep = 'year'))

# 4. EXTRACT ONE STATE ----
extract.state = function(state)
{
    file = raw.file(state)
    # Checked again here in case the file appeared after section 2 (e.g. another run)
    if (file.exists(file) && !FORCE.EXTRACT)
        return(paste0(state, ': already saved'))

    sim.file = simset.file(state)
    if (!file.exists(sim.file))
        stop("No simset file: ", sim.file)

    t0 = Sys.time()
    simset = load.simulation.set(sim.file)
    if (!is.null(N.SIM.KEEP))
        simset = simset$thin(n = N.SIM.KEEP)

    # Every year from MODEL.FROM.YEAR (or the file's first year, if later) to the file's last year
    yrs = as.character(max(MODEL.FROM.YEAR, simset$from.year):simset$to.year)
    get.one = function(outcome, keep, output)
        simset$get(outcomes = outcome, keep.dimensions = keep, output = output,
                   dimension.values = list(year = yrs), drop.single.sim.dimension = F)

    outcomes = list()
    problems = character()
    for (o in names(RAW.OUTCOMES))
    {
        spec = RAW.OUTCOMES[[o]]
        outcomes[[o]] = tryCatch({
            if (spec$type == 'count')
                list(value = get.one(o, spec$keep, 'value'))
            else
                list(numerator = get.one(o, spec$keep, 'numerator'),
                     denominator = get.one(o, spec$keep, 'denominator'))
        }, error = function(e) {
            problems[o] <<- conditionMessage(e)
            NULL
        })
    }

    info = list(state = state, simset.file = sim.file, simset.years = c(simset$from.year, simset$to.year),
                years.saved = range(as.numeric(yrs)), n.sim = simset$n.sim, created = Sys.time())
    rm(simset); gc()

    # Write under a temporary name, then rename: <state>.rds exists only once it is complete
    tmp.file = paste0(file, '.tmp')
    saveRDS(list(info = info, outcomes = outcomes, problems = problems), tmp.file)
    if (!file.rename(tmp.file, file))
        stop("Could not rename ", tmp.file, " to ", file)
    paste0(state, ': saved years ', min(yrs), '-', max(yrs), ', ', info$n.sim, ' sims, ',
           round(as.numeric(Sys.time() - t0, units = 'secs')), ' s',
           if (length(problems)) paste0(' | PROBLEMS: ', paste(names(problems), collapse = ', ')) else '')
}

# 5. RUN ALL STATES ----
# One failed state is reported and skipped; the others keep going.
run.one = function(state)
    tryCatch(extract.state(state),
             error = function(e) paste0(state, ': FAILED - ', conditionMessage(e)))

if (length(STATES.TO.RUN) == 0) {
    print("Nothing to extract: all states are already saved. Set FORCE.EXTRACT = T to redo them.")
} else {
    t0 = Sys.time()
    if (N.CORES > 1) {
        status = parallel::mclapply(STATES.TO.RUN, run.one, mc.cores = N.CORES, mc.preschedule = F)
    } else {
        status = lapply(STATES.TO.RUN, run.one)
    }
    print(unlist(status))
    print(paste0("Step 1 took ", round(as.numeric(Sys.time() - t0, units = 'mins'), 1), " minutes"))
}
