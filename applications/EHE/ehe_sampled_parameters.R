

EHE.SAMPLED.PARAMETERS.DISTRIBUTION = join.distributions(
  future.testing.slope = Lognormal.Distribution(0, log(1.05)/2),
  future.testing.slope.after.year = Uniform.Distribution(2025,2031),
  
  future.suppression.slope = Lognormal.Distribution(0, log(1.05)/2),
  future.suppression.slope.after.year = Uniform.Distribution(2025,2031),
  
  future.prep.slope = Lognormal.Distribution(0, log(1.05)/2),
  future.prep.slope.after.year = Uniform.Distribution(2025,2031)
)

EHE.APPLY.SAMPLED.PARAMETERS.FN <- function(model.settings, parameters)
{
  model.settings$set.element.functional.form.future.slope.after.time(element.name = 'general.population.testing.without.covid',
                                                                     after.time = parameters['future.testing.slope.after.year'])
  model.settings$set.element.functional.form.future.slope(element.name = 'general.population.testing.without.covid',
                                                          slope = parameters['future.testing.slope'])
  
  
  model.settings$set.element.functional.form.future.slope.after.time(element.name = 'suppression.of.diagnosed.without.covid',
                                                                     after.time = parameters['future.suppression.slope.after.year'])
  model.settings$set.element.functional.form.future.slope(element.name = 'suppression.of.diagnosed.without.covid',
                                                          slope = parameters['future.suppression.slope'])
  
  model.settings$set.element.functional.form.future.slope.after.time(element.name = 'oral.prep.uptake.without.covid',
                                                                     after.time = parameters['future.prep.slope.after.year'])
  model.settings$set.element.functional.form.future.slope(element.name = 'oral.prep.uptake.without.covid',
                                                          slope = parameters['future.prep.slope'])

}