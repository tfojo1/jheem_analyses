################################################################################
## ADAP LOSS GRID INTERVENTION SPECIFICATION
################################################################################

print("Sourcing Ryan White intervention specifications")

source('../jheem_analyses/applications/ryan_white/ryan_white_main.R')


################################################################################
## GRID SPECIFICATION
################################################################################

ADAP.START.YEAR <- 2026 + 2/12

# Full grid, including existing 100% scenario
LOSE.ADAP.GRID <- seq(0, 1, by = 0.05)


################################################################################
## CREATE GRID INTERVENTIONS
################################################################################

for (lose_fraction in LOSE.ADAP.GRID) {
    
    ## Fixed fraction across posterior simulations
    lose.adap.fraction.grid <- rep(lose_fraction, N.SIM)
    
    dim(lose.adap.fraction.grid) <- c(1, N.SIM)
    
    dimnames(lose.adap.fraction.grid) <- list(
        'lose.adap.fraction',
        NULL
    )
    
    
    ###########################################################################
    ## EXPANSION STATES
    ###########################################################################
    
    adap.cessation.expansion.effect.grid <- create.intervention.effect(
        quantity.name = 'adap.suppression.expansion.effect',
        start.time = ADAP.START.YEAR,
        effect.values = expression(
            1 - lose.adap.fraction * lose.adap.expansion.effect
        ),
        apply.effects.as = 'value',
        scale = 'proportion',
        times = ADAP.START.YEAR + LOSS.LAG,
        allow.values.less.than.otherwise = TRUE,
        allow.values.greater.than.otherwise = FALSE
    )
    
    
    ###########################################################################
    ## NON-EXPANSION STATES
    ###########################################################################
    
    adap.cessation.nonexpansion.effect.grid <- create.intervention.effect(
        quantity.name = 'adap.suppression.nonexpansion.effect',
        start.time = ADAP.START.YEAR,
        effect.values = expression(
            1 - lose.adap.fraction * lose.adap.nonexpansion.effect
        ),
        apply.effects.as = 'value',
        scale = 'proportion',
        times = ADAP.START.YEAR + LOSS.LAG,
        allow.values.less.than.otherwise = TRUE,
        allow.values.greater.than.otherwise = FALSE
    )
    
    
    ###########################################################################
    ## INTERVENTION CODE
    ###########################################################################
    
    intervention.code <- paste0(
        "adap.",
        sprintf("%03d", round(lose_fraction * 100)),
        ".end",
        rw.intervention.suffix
    )
    
    
    ###########################################################################
    ## CREATE INTERVENTION
    ###########################################################################
    
    create.intervention(
        adap.cessation.expansion.effect.grid,
        adap.cessation.nonexpansion.effect.grid,
        parameters = rbind(
            RW.effect.values[c(1, 4), ],
            lose.adap.fraction.grid
        ),
        WHOLE.POPULATION,
        code = intervention.code
    )
    
    
    print(
        paste0(
            "Created intervention: ",
            intervention.code,
            " | lose.adap.fraction = ",
            lose_fraction
        )
    )
}


################################################################################
## GRID INTERVENTION CODES
################################################################################

ADAP.GRID.INTERVENTION.CODES <- paste0(
    "adap.",
    sprintf("%03d", round(LOSE.ADAP.GRID * 100)),
    ".end",
    rw.intervention.suffix
)

print("ADAP loss grid interventions specified:")

print(ADAP.GRID.INTERVENTION.CODES)