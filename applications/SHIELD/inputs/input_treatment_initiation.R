# •	“Among 880 patients, 592 (67%) were treated 7 days or less after evaluation; 711 (80%) were treated 14 days or less after evaluation; 
# 764 (86%) were treated 21 days or less after evaluation; 786 (89%) were treated 30 days or less after evaluation; 
# 809 (92%) patients were treated 90 days or less after evaluation; 
# 3 (<1%) were treated more than 90 days after evaluation; 
# and 72 (8%) had no reported treatment.” 


#'@PK: to double check these: 

treated=list(n=c(592,119,53,22,23,3,72), 
             days=c(7,14,21,30,90,NA,NA))

p=(treated$n/880)[-1]
t=(treated$days/365)[-1]
plot(y=cumsum(p),x=t,type="l")
lm(-log(1-p)~t+0)

p.immediate.treatment=592/880 #udner a week
p.delayed.treatment=(119+52+22+23+3)/880 #under 3 months
rate.delayed.treatment=p.delayed.treatment/0.3

not.treated=880-592
72/not.treated
