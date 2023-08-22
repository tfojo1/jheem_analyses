

get.linkage.model = function(specification.metadata,
                             location){
  get.continuum.model(continuum.manager = ALL.DATA.MANAGERS$continuum,
                      type='linkage',
                      location=location)
}

get.naive.to.suppressed.model = function(specification.metadata,
                                         location){
  get.continuum.model(continuum.manager = ALL.DATA.MANAGERS$continuum,
                      type='naive.to.suppressed',
                      location=location)
}

get.time.to.start.art.model = function(specification.metadata,
                                       location){
  get.continuum.model(continuum.manager = ALL.DATA.MANAGERS$continuum,
                      type='start.art',
                      location=location)
}

get.disengagement.naive.model = function(specification.metadata,
                                         location){
  get.continuum.model(continuum.manager = ALL.DATA.MANAGERS$continuum,
                      type='naive.to.disengaged',
                      location=location)
}

get.disengagement.failing.model = function(specification.metadata,
                                           location){
  get.continuum.model(continuum.manager = ALL.DATA.MANAGERS$continuum,
                      type='failing.to.disengaged',
                      location=location)
}

get.disengagement.recently.suppressed.model = function(specification.metadata,
                                                       location){
  get.continuum.model(continuum.manager = ALL.DATA.MANAGERS$continuum,
                      type='recently.suppressed.to.disengaged',
                      location=location)
}

get.disengagement.durably.suppressed.model = function(specification.metadata,
                                                      location){
  get.continuum.model(continuum.manager = ALL.DATA.MANAGERS$continuum,
                      type='durably.suppressed.to.disengaged',
                      location=location)
}

get.reengagement.model = function(specification.metadata,
                                  location){
  get.continuum.model(continuum.manager = ALL.DATA.MANAGERS$continuum,
                      type='reengagement',
                      location=location)
}

get.gain.of.suppression.model = function(specification.metadata,
                                         location){
  get.continuum.model(continuum.manager = ALL.DATA.MANAGERS$continuum,
                      type='failing.to.suppressed',
                      location=location)
}

get.loss.of.suppression.recent.model = function(specification.metadata,
                                                location){
  get.continuum.model(continuum.manager = ALL.DATA.MANAGERS$continuum,
                      type='recently.suppressed.to.failing',
                      location=location)
}

get.loss.of.suppression.durable.model = function(specification.metadata,
                                                 location){
  get.continuum.model(continuum.manager = ALL.DATA.MANAGERS$continuum,
                      type='durably.suppressed.to.failing',
                      location=location)
}


