
test.cdc.test.positivity.likelihood.instr = create.custom.likelihood.instructions(
    name = 'test.likelihood',
    compute.function = function(sim, log=T){
        
        dnorm(as.numeric(sim$get('total.cdc.hiv.test.positivity', year='2019'), 0.001, 0.2, log=T))
        
    }
)
