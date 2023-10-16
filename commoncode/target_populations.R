

WHOLE.POPULATION = create.target.population(name = 'Whole Population')

# By Race
BLACK = create.target.population(race='black', name='Black')
HISPANIC = create.target.population(race='hispanic', name='Hispanic')
OTHER.RACE = create.target.population(race='other', name='Other')
NON.BLACK.NON.HISPANIC = create.target.population(race=c('black','hispanic'), invert = T, name='Non-Black, Non-Hispanic')

# By Race, with "to" suffix
BLACK.TO = create.target.population(race.to='black', name='Black')
HISPANIC.TO = create.target.population(race.to='hispanic', name='Hispanic')
OTHER.RACE.TO = create.target.population(race.to='other', name='Other')
NON.BLACK.NON.HISPANIC.TO = create.target.population(race.to=c('black','hispanic'), invert = T, name='Non-Black, Non-Hispanic')