
#'@title Check For Needed Mappings
#'@description Check a data manager and a model ontology for any pairs of ontologies that can't be aligned given the registered ontologies mappings.
#'@details Note that this only checks if pairs of ontologies can be aligned with one another (with 'get.mappings.to.align.ontologies()'), not whether each one can be mapped unidirectionally to the other (which would be with 'get.ontology.mapping()').
#'@param model.ontology The model ontology for a given specification
#'@param dimension.groupings A list of sets of one or more dimensions that are expected to map together. Helps create informative output
check.for.needed.mappings <- function(data.manager,
                                      model.ontology,
                                      dimension.groupings = list(c('race', 'ethnicity'),
                                                              c('sex', 'risk'))) {
    # browser()
    model.ont.by.grouping = lapply(dimension.groupings, function(dimensions) {
        dimensions.in.model.ont = intersect(dimensions, names(model.ontology))
        model.ontology[dimensions.in.model.ont]
    })
    
    report.by.outcome = lapply(data.manager$outcomes, function(outcome) {
        ontologies.this.outcome = data.manager$get.ontologies.for.outcome(outcome)
        
        report.by.dimension.grouping = lapply(seq_along(dimension.groupings), function(grouping.number) {
            
            ontologies.this.grouping = c(list(model=model.ont.by.grouping[[grouping.number]]),
                                         lapply(ontologies.this.outcome, function(ont) {
                                             dimensions.in.this.ont = intersect(dimension.groupings[[grouping.number]], names(ont))
                                             ont[dimensions.in.this.ont]
                                         }))
            
            if (length(ontologies.this.grouping) < 2) return(NULL)
            
            # Pairs of indices
            combs = combn(length(ontologies.this.grouping), 2)
            
            # Return if we could find mappings to align each pair, because if we can align each pair, we can align each set of many
            pairs.cannot.be.mapped = apply(combs, 2, function(indices) {
                missing.mapping = is.null(get.mappings.to.align.ontologies(ontologies.this.grouping[[indices[[1]]]], ontologies.this.grouping[[indices[[2]]]]))
            })
            unmappable.pairs = combs[,pairs.cannot.be.mapped, drop=F]
            if (length(unmappable.pairs)==0) return(NULL)

            return(apply(unmappable.pairs, 2, function(indices) {paste0(names(ontologies.this.grouping)[indices], collapse="__")}))
        })
    })
    names(report.by.outcome) = data.manager$outcomes
    
    # Find pairs of onts
    ont.pairs = unique(unlist(report.by.outcome))
    
    # By dimension grouping (2nd layer), by ont pair, find which outcomes the ont pair occurs in
    report.by.grouping = lapply(seq_along(dimension.groupings), function(grouping.number) {

        x=lapply(ont.pairs, function(ont.pair) {

            x=sapply(report.by.outcome, function(outcome) {
                ont.pair %in% outcome[[grouping.number]]
            })
            names(report.by.outcome)[x]
        })
        was.found = sapply(x, length)!=0
        setNames(x[was.found], ont.pairs[was.found])
    })
    names(report.by.grouping) = sapply(dimension.groupings, function(grouping) {paste0(grouping, collapse="__")})

    # Print out a useful summary
    for (grouping.number in seq_along(dimension.groupings)) {
        
        if (length(report.by.grouping[[grouping.number]])==0)
            cat("No missing mappings expected for dimension grouping '", names(report.by.grouping)[[grouping.number]], "'", "\n")
        
        else {
            
            cat("Expected mappings were missing for dimension grouping '", names(report.by.grouping)[[grouping.number]], "':\n")
            for (ont.pair in names(report.by.grouping[[grouping.number]]))
            {
                parsed.ont.pair = strsplit(ont.pair, "__")[[1]]
                cat(".....Ontologies", parsed.ont.pair[1], "and", parsed.ont.pair[2], "could not be aligned; these are found in outcome(s):", "\n     .....")
                cat(report.by.grouping[[grouping.number]][[ont.pair]], sep="\n     .....")
            }
        }
    }
    invisible(report.by.grouping)
}

# Function to call on a data manager, first without a target (model ontology)
# We want to check if all ontologies for each outcome can be aligned with one another
# We can probably prove that if every pair of ontologies can be aligned, then every set of more than two ontologies (like the pull does to get a 'universal ontology') can be aligned, too.

# model.ont = ontology(race=c('black', 'hispanic', 'other'),
#                      sex=c('heterosexual_male', 'female', 'msm'),
#                      risk = c('never_IDU', 'active_IDU', 'IDU_in_remission'))
