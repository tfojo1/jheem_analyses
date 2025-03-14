codes = c('pop.ehe',
          'trans.ehe',
          'full.ehe',
          'final.ehe')

CHAINS = setNames(list(1, 1, 1, 1:4), codes)
shortCodes = setNames(c('pop', 'trans', 'full', 'final'), codes)

SPEC = "applications/EHE/ehe_specification.R"
REG = "applications/EHE/calibration_runs/ehe_register_calibrations.R"

for (code in codes) {
    make.setup.scripts(MSAS.OF.INTEREST, 'ehe', code, SPEC, REG)
    make.run.scripts(MSAS.OF.INTEREST, 'ehe', code, chains = CHAINS[[code]], SPEC, REG)
    make.setup.master.script(paste0('az_S_redo_', shortCodes[code], "_031425"), MSAS.OF.INTEREST, 'ehe', code, overwrite = T)
    make.run.master.script(paste0('az_R_redo_', shortCodes[code], "_031425"), MSAS.OF.INTEREST, 'ehe', code, chains = CHAINS[[code]], overwrite = T)
}