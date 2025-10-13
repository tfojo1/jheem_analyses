df.a = read.csv('applications/ryan_white/ryan_white_data/costs/FY23/FY23_part_a_costs_by_location.csv')
df.a = df.a[-nrow(df.a),] # remove total row
df.a$state = substr(df.a[,1], nchar(df.a[,1])-1, nchar(df.a[,1]))
df.a$minority.cost = as.numeric(gsub("[$, ]", "", df.a$FY.2023.Final.MAI.Award))
df.a$cost = as.numeric(gsub("[$, ]", "", df.a$Total.FY.2023.Part.A.Award))

df.b = read.csv('applications/ryan_white/ryan_white_data/costs/FY23/FY23_part_b_costs_by_location.csv')
df.b$state = unlist(locations::get.location.code(df.b[,1], types = 'STATE'))
df.b = df.b[!is.na(df.b$state),] # Gets rid of totals and some small territories
df.b$adap.cost = as.numeric(gsub("[$, ]", "", df.b$FY.2023.ADAP.Award)) + 
    as.numeric(gsub("[$, ]", "", df.b$FY.2023.ADAP.Supplemental.Award))
df.b$minority.cost = as.numeric(gsub("[$, ]", "", df.b$FY.2023.Minority.AIDS.Initiative.Award))
df.b$total.cost = as.numeric(gsub("[$, ]", "", df.b$FY.2023.Total.Part.B.Funding))
df.b$non.adap.cost = df.b$total.cost - df.b$adap.cost

df.c.eis = read.csv('applications/ryan_white/ryan_white_data/costs/FY23/FY23_part_c_eis_costs_by_location.csv')
df.c.eis$state = df.c.eis$State
df.c.eis$cost = as.numeric(gsub("[$, ]", "", df.c.eis$FY.2023.Part.C.EIS.Award))

df.c.capacity = read.csv('applications/ryan_white/ryan_white_data/costs/FY23/FY23_part_c_capacity_costs_by_location.csv')
df.c.capacity = df.c.capacity[-nrow(df.c.capacity),] # remove total row
df.c.capacity$state = df.c.capacity$State
df.c.capacity$cost = as.numeric(gsub("[$, ]", "", df.c.capacity$FY.2023.Award))

df.d = read.csv('applications/ryan_white/ryan_white_data/costs/FY23/FY23_part_d_costs_by_location.csv')
df.d = df.d[-nrow(df.d),] # remove empty last row
df.d$state = df.d$State
df.d$cost = as.numeric(gsub("[$, ]", "", df.d$FY.2023.Total.Award))

df.ehe = read.csv('applications/ryan_white/ryan_white_data/costs/FY23/FY23_ehe_costs_by_location.csv')
df.ehe$state.from.name = unlist(locations::get.location.code(df.ehe[,1], types = 'STATE'))
df.ehe$state.from.city = substr(df.ehe[,1], nchar(df.ehe[,1])-5, nchar(df.ehe[,1])-4)
df.ehe$state = df.ehe$state.from.city
df.ehe$state[!is.na(df.ehe$state.from.name)] = df.ehe$state.from.name[!is.na(df.ehe$state.from.name)]
df.ehe$cost = as.numeric(gsub("[$, ]", "", df.ehe$Award.Amount))

states = unique(c(
    df.a$state,
    df.b$state,
    df.c.eis$state,
    df.c.capacity$state,
    df.d$state,
    df.ehe$state
))

state.rw.costs = cbind(
    part.a = sapply(states, function(st){sum(df.a$cost[df.a$state==st])}),
    part.a.minority = sapply(states, function(st){sum(df.a$minority.cost[df.a$state==st])}),
    part.b = sapply(states, function(st){sum(df.b$total.cost[df.b$state==st])}),
    part.b.adap = sapply(states, function(st){sum(df.b$adap.cost[df.b$state==st])}),
    part.b.non.adap = sapply(states, function(st){sum(df.b$non.adap.cost[df.b$state==st])}),
    part.b.minority = sapply(states, function(st){sum(df.b$minority.cost[df.b$state==st])}),
    part.c.eis = sapply(states, function(st){sum(df.c.eis$cost[df.c.eis$state==st])}),
    part.c.capacity = sapply(states, function(st){sum(df.c.capacity$cost[df.c.capacity$state==st])}),
    part.d = sapply(states, function(st){sum(df.d$cost[df.d$state==st])}),
    ehe = sapply(states, function(st){sum(df.ehe$cost[df.ehe$state==st])})
)

state.rw.costs = cbind(state.rw.costs,
                       total = rowSums(state.rw.costs[,c('part.a','part.b','part.c.eis','part.c.capacity','part.d','ehe')]))

state.rw.fraction.c.d.ehe.minority.of.non.adap = rowSums(state.rw.costs[,c('part.a.minority','part.b.minority','part.c.eis','part.c.capacity','part.d','ehe')]) /
    (state.rw.costs[,'total'] - state.rw.costs[,'part.b.adap'])

state.rw.fraction.ehe.of.non.adap = state.rw.costs[,'ehe'] / (state.rw.costs[,'total'] - state.rw.costs[,'part.b.adap'])