register.transmute.calibration.info(transmute.code = 'cdcp',
                                    from.version = 'ehe',
                                    to.version = 'cdcp',
                                    n.iter.first.sim = 1000,
                                    n.iter.subsequent.sims = 100,
                                    likelihood.instructions = cdc.prep.joint.likelihood.instructions,
                                    prior.distribution = c(CDC.PREP.PARAMETERS.PRIOR,CDC.TESTING.PARAMETERS.PRIOR),
                                    sampling.blocks = CDC.PREP.PARAMETER.SAMPLING.BLOCKS )
