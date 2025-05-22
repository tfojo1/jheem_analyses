
# reported syphilis diagnoses in 1970
n.diag= c(
    total=91382 ,
    ps=21981,
    el=16311,
    ll=50348,
    congenital=1953)

known.stages=sum(n.diag[c("ps","el","ll","congenital")])
known.stages #this is short of the total diagnoses 91,382

#rates per 100,000
r.diag=c(
    total=44.8 ,
    ps=10.8,
    el=8,
    ll=24.7,
    congenital=52.3)



#assumong 1/4 of ps are in primary stage and the rest are secondary stage based on duration
r.diag.primary=1/4*r.diag["ps"]
r.diag.secondary=3/4*r.diag["ps"]
r.diag.el=r.diag["el"]
# for those with latelatent/unknown duration: assuming 25% are tertiary
r.diag.tertiary=1/4*r.diag["ll"]
r.diag.ll=3/4*r.diag["ll"]

print("estimated rate of syphilis diagnoses by stage (per 100,000 p) in 1970:")
print(paste(" primary= " , r.diag.primary, 
            " secondary= ",  r.diag.secondary,
            " el= ",r.diag.el,
            " ll= ", r.diag.ll,
            " tertiary= ", r.diag.tertiary,
            " congenital= ", r.diag["congenital"]))


