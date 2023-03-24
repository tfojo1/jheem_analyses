

create.comorbidities.manager <- function()
{
    rv = list()
    class(rv) = 'comorbidities.manager'
    
    rv
}

get.comorbidities.model <- function(comorbidities.manager,
                                    type,
                                    location)
{
    base.model = comorbidities.manager[[type]]
    
    
    # Creating a new object below lets us use the most up-to-date code for models
    create.logistic.model(intercept = base.model$intercept,
                          slope = base.model$slope,
                          anchor.year = base.model$anchor.year,
                          min.proportion = base.model$min.proporiton,
                          max.proportion = base.model$max.proportion,
                          log.ors = base.model$log.orgs)
}

