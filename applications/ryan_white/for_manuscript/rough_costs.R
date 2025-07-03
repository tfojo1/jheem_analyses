
# Part A costs, 2023
#  https://ryanwhite.hrsa.gov/about/parts-and-initiatives/part-a/fy-2023-grant-awards

# Part B costs, 2023
#  https://ryanwhite.hrsa.gov/about/parts-and-initiatives/part-b-adap/fy-2023-grant-awards

# Part C costs, 2023
#  https://ryanwhite.hrsa.gov/about/parts-and-initiatives/part-c-early-intervention/fy-2023-capacity-development-awards

# Part D costs, 2023
#  https://ryanwhite.hrsa.gov/about/parts-and-initiatives/part-d-swic/fy-2023-grant-awards

# Part F costs, 2023
#  https://ryanwhite.hrsa.gov/about/parts-and-initiatives/part-f-dental-programs/fy24-grant-awards

source('../jheem_analyses/commoncode/locations_of_interest.R')
#source('../jheem_analyses/applications/ryan_white/ryan_white_main.R')

#-- Some explicit location mappings --#

explicit.cbsa.mappings = rbind(
    c("West Palm Beach", "FL", "C.33100"),
    c("Secaucus", "NJ", "C.35620"),
    c("Mineola", "NY", "C.35620"),
    c("New Brunswick", "NJ", "C.35620"),
    c("Paterson", "NJ", "C.35620"),
    c("Brooklyn", "NY", "C.35620"),
    c("Bronx", "NY", "C.35620"),
    c("Elmhurst", "NY", "C.35620"),
    c("Manhasset", "NY", "C.35620"),
    c("Bronx", "NY", "C.35620"),
    c("Stony Brook", "NY", "C.41740"),
    c("La Jolla", "CA", "C.35620"),
    c("Winst.n.Salem", "NC", "C.49180"),
    c("Roxbury", "MA", "C.14460"),
    c("Commerce", "CA", "C.31080"),
    c("Chester", "PA", "C.37980"),
    c("Hyattsville", "MD", "C.47900"),
    c("Claremont", "CA", "C.31080"),
    c("Plainfield", "NJ", "C.35620"),
    c("Asbury Park", "NJ", "C.35620"),
    c("Loma Linda", "CA", "C.40140"),
    c("Pomona", "CA", "C.31080"),
    c("Hackensack", "NJ", "C.35620"),
    c("Edison", "NJ", "C.35620"),
    c("Flushing", "NY", "C.35620"),
    c("Jamaica", "NY", "C.35620"),
    c("Valhalla", "NY", "C.35620")
)

apply.explicit.mappings <- function(df, city.name, state)
{
    for (i in 1:nrow(explicit.cbsa.mappings))
    {
        row = explicit.cbsa.mappings[i,]
        df$cbsa[is.na(df$cbsa) & grepl(row[1], city.name) & grepl(row[2], state)] = row[3]
    }
    
    df
}

parse.costs = function(costs)
{
    as.numeric(gsub("\\$","",
                    gsub(",", "", 
                         gsub(" ", "", costs))))
}

#-- Read in the costs --#

COSTS.DIR = '../jheem_analyses/applications/ryan_white/ryan_white_data/costs'

# Prep Part A
df.a = read.csv(file.path(COSTS.DIR, 'ryan_white_part_a_costs_by_location.csv'), stringsAsFactors = F)
df.a$cost = parse.costs(df.a[,ncol(df.a)])
df.a$cbsa = suppressWarnings(locations::get.cbsa.for.msa.name(df.a[,1]))
df.a = apply.explicit.mappings(df=df.a, city.name = df.a[,1], state=df.a[,1])

df.a[is.na(df.a$cbsa),]

# Prep Part B
df.b = read.csv(file.path(COSTS.DIR, 'ryan_white_part_b_costs_by_location.csv'), stringsAsFactors = F)
df.b$cost = parse.costs(df.b[,ncol(df.b)])
df.b$State = locations::get.location.code(df.b[,1], types = 'STATE')

# Prep Part C
df.c = read.csv(file.path(COSTS.DIR, 'ryan_white_part_c_costs_by_location.csv'), stringsAsFactors = F)
df.c$cost = parse.costs(df.c[,ncol(df.c)])
df.c$cbsa = suppressWarnings(locations::get.cbsa.for.msa.name(paste0(df.c[,2], ", ", df.c[,3])))
df.c = apply.explicit.mappings(df=df.c, city.name = df.c[,2], state=df.c[,3])

df.c[is.na(df.c$cbsa),c('City',"State","cbsa")]

# Prep Part D
df.d = read.csv(file.path(COSTS.DIR, 'ryan_white_part_d_costs_by_location.csv'), stringsAsFactors = F)
df.d$cost = parse.costs(df.d[,ncol(df.d)])
df.d$cbsa = suppressWarnings(locations::get.cbsa.for.msa.name(paste0(df.d[,2], ", ", df.d[,3])))
df.d = apply.explicit.mappings(df=df.d, city.name = df.d[,2], state=df.d[,3])

df.d[is.na(df.d$cbsa),c('City',"State","cbsa")]

# Prep Part F
df.f.1 = read.csv(file.path(COSTS.DIR, 'ryan_white_part_f_costs_by_location_1.csv'), stringsAsFactors = F)
df.f.1$cost = parse.costs(df.f.1[,ncol(df.f.1)])
df.f.1$cbsa = suppressWarnings(locations::get.cbsa.for.msa.name(paste0(df.f.1[,2], ", ", df.f.1[,3])))
df.f.1 = apply.explicit.mappings(df=df.f.1, city.name = df.f.1[,2], state=df.f.1[,3])

df.f.1[is.na(df.f.1$cbsa),c('City',"State","cbsa")]


df.f.2 = read.csv(file.path(COSTS.DIR, 'ryan_white_part_f_costs_by_location_2.csv'), stringsAsFactors = F)
df.f.2$cost = parse.costs(df.f.2[,ncol(df.f.2)])
df.f.2$cbsa = suppressWarnings(locations::get.cbsa.for.msa.name(paste0(df.f.2[,2], ", ", df.f.2[,3])))
df.f.2 = apply.explicit.mappings(df=df.f.2, city.name = df.f.2[,2], state=df.f.2[,3])

df.f.2[is.na(df.f.2$cbsa),c('City',"State","cbsa")]


#-- Put it together for cities --#

surv.year = '2021'
rw.year = '2023'
RW.CITY.COSTS = sapply(c('a','b','c','d','f'), function(part){
    sapply(RW.CITIES, function(loc){
        
        if (part=='b')
        {
            states = locations::get.overlapping.locations(loc, 'STATE')
            counties = locations::get.contained.locations(loc, 'COUNTY')
            n.pwh.in.msa = sum(SURVEILLANCE.MANAGER$data$diagnosed.prevalence$estimate$cdc.hiv$cdc$year__location[surv.year, counties], na.rm=T)
            n.rw.clients.in.msa = RW.DATA.MANAGER$data$non.adap.clients$estimate$ryan.white.program$ryan.white.pdfs$year__location[rw.year,loc]
    
            
            p.pwh.in.state = sapply(states, function(st){
                if (length(states)==1)
                    1
                else
                {
                    counties.in.state.and.msa = intersect(
                        counties,
                        locations::get.contained.locations(st, "COUNTY")
                    )
                    
                    n.pwh.in.state.and.msa = sum(SURVEILLANCE.MANAGER$data$diagnosed.prevalence$estimate$cdc.hiv$cdc$year__location[surv.year, counties.in.state.and.msa], na.rm=T)
                    
                    n.pwh.in.state.and.msa / n.pwh.in.msa
                }
            })
            
            n.rw.clients.in.state = RW.DATA.MANAGER$data$non.adap.clients$estimate$ryan.white.program$ryan.white.pdfs$year__location[rw.year,states]     
            est.n.rw.clients.in.msa.and.state = n.rw.clients.in.msa * p.pwh.in.state
            
            state.costs = sapply(states, function(st){
                sum(df.b$cost[df.b$State==st])
            })
            
            sum(state.costs * est.n.rw.clients.in.msa.and.state / n.rw.clients.in.state)
        }
        else
        {
            if (part=='a')
                dfs = list(df.a)
            else if (part=='c')
                dfs = list(df.c)
            else if (part=='d')
                dfs = list(df.c)
            else if (part=='f')
                dfs = list(df.f.1, df.f.2)
            
            sum(sapply(dfs, function(df){
                tryCatch({
                    sum(df$cost[!is.na(df$cbsa) & df$cbsa==loc])
                },
                error = function(e){
                    browser()
                })
            }))
        }
    })
})

save(RW.CITY.COSTS, file='../jheem_analyses/applications/ryan_white/rw_costs.Rdata')
