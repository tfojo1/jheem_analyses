
# Using data from 2000, 2002, and 2003 (2001 values seem off), 
# calculating the ratio of AIDS diagnoses:HIV diagnoses in states that have at least 100 HIV diagnoses 


# aids.diagnoses.multiplier
    # 1.044985
# aids.diagnoses.sd 
    # 0.2306945

# only including states with >=100 cases 
hiv.cases.2000 = c("Alabama" = 498,
                   "Arizona" = 450,
                   "Arkansas" = 245,
                   "Colorado" = 200,
                   "Florida" = 5810,
                   "Indiana" = 319,
                   "Louisiana" = 841,
                   "Michigan" = 644,
                   "Minnesota" = 227,
                   "Mississippi" = 459,
                   "Missouri" = 348,
                   "Nevada" = 296,
                   "New Jersey" = 1455,
                   "New York" = NA,
                   "North Carolina" = 1011,
                   "Ohio" = 601,
                   "Oklahoma" = 297,
                   "South Carolina" = 597,
                   "Tennessee" = 846,
                   "Texas" = 4204,
                   "Virginia" = 769,
                   "Wisconsin" = 198)

aids.cases.2000 = c("Alabama" = 483,
                   "Arizona" = 460,
                   "Arkansas" = 194,
                   "Colorado" =313,
                   "Florida" = 4976,
                   "Indiana" = 389,
                   "Louisiana" = 679,
                   "Michigan" = 767,
                   "Minnesota" = 185,
                   "Mississippi" = 431,
                   "Missouri" = 459,
                   "Nevada" = 286,
                   "New Jersey" = 1929,
                   "New York" = NA,
                   "North Carolina" = 696,
                   "Ohio" = 599,
                   "Oklahoma" = 352,
                   "South Carolina" = 810,
                   "Tennessee" = 863,
                   "Texas" = 2667,
                   "Virginia" = 891,
                   "Wisconsin" = 218)

hiv.cases.2001 = c("Alabama" = 491,
                   "Arizona" = 553,
                   "Arkansas" = 211,
                   "Colorado" = 391,
                   "Florida" = 5744,
                   "Indiana" = 394,
                   "Louisiana" = 830,
                   "Michigan" = 674,
                   "Minnesota" = 272,
                   "Mississippi" = 408,
                   "Missouri" = 481,
                   "Nevada" = 297,
                   "New Jersey" = 1571,
                   "New York" = 13403,
                   "North Carolina" = 1046,
                   "Ohio" = 831,
                   "Oklahoma" = 223,
                   "South Carolina" = 695,
                   "Tennessee" = 721,
                   "Texas" = 4237,
                   "Virginia" = 879,
                   "Wisconsin" = 165)

aids.cases.2001 = c("Alabama" = 483,
                    "Arizona" = 540,
                    "Arkansas" = 199,
                    "Colorado" = 288,
                    "Florida" = 5138,
                    "Indiana" = 378,
                    "Louisiana" = 861,
                    "Michigan" = 548,
                    "Minnesota" = 157,
                    "Mississippi" = 418,
                    "Missouri" = 445,
                    "Nevada" = 252,
                    "New Jersey" = 1756,
                    "New York" = 7476,
                    "North Carolina" = 942,
                    "Ohio" = 581,
                    "Oklahoma" = 243,
                    "South Carolina" = 729,
                    "Tennessee" = 602,
                    "Texas" = 2892,
                    "Virginia" = 951,
                    "Wisconsin" = 193)

hiv.cases.2002 = c("Alabama" = 522,
                   "Arizona" = 499,
                   "Arkansas" = 197,
                   "Colorado" = 437,
                   "Florida" = 6122,
                   "Indiana" = 477,
                   "Louisiana" = 901,
                   "Michigan" = 584,
                   "Minnesota" = 250,
                   "Mississippi" = 383,
                   "Missouri" = 316,
                   "Nevada" = 323,
                   "New Jersey" = 1208,
                   "New York" = 11459,
                   "North Carolina" = 1118,
                   "Ohio" = 876,
                   "Oklahoma" = 197,
                   "South Carolina" = 597,
                   "Tennessee" = 797,
                   "Texas" = 5027,
                   "Virginia" = 999,
                   "Wisconsin" = 176)

aids.cases.2002 = c("Alabama" = 432,
                    "Arizona" = 630,
                    "Arkansas" = 240,
                    "Colorado" = 332,
                    "Florida" = 5058,
                    "Indiana" = 491,
                    "Louisiana" = 1167,
                    "Michigan" = 789,
                    "Minnesota" = 161,
                    "Mississippi" = 433,
                    "Missouri" = 391,
                    "Nevada" = 314,
                    "New Jersey" = 1436,
                    "New York" = 6664,
                    "North Carolina" = 1061,
                    "Ohio" = 780,
                    "Oklahoma" = 204,
                    "South Carolina" = 833,
                    "Tennessee" = 792,
                    "Texas" = 3140,
                    "Virginia" = 955,
                    "Wisconsin" = 187)

hiv.cases.2003 = c("Alabama" = 501,
                   "Arizona" = 510,
                   "Arkansas" = 183,
                   "Colorado" = 365,
                   "Florida" = 5467,
                   "Indiana" = 336,
                   "Louisiana" = 787,
                   "Michigan" = 548,
                   "Minnesota" = 225,
                   "Mississippi" = 354,
                   "Missouri" = 467,
                   "Nevada" = 221,
                   "New Jersey" = 1361,
                   "New York" = 8403,
                   "North Carolina" = 1315,
                   "Ohio" = 786,
                   "Oklahoma" = 206,
                   "South Carolina" = 539,
                   "Tennessee" = 696,
                   "Texas" = 4292,
                   "Virginia" = 723,
                   "Wisconsin" = 172)

aids.cases.2003 = c("Alabama" = 472,
                    "Arizona" = 614,
                    "Arkansas" = 188,
                    "Colorado" = 366,
                    "Florida" = 4666,
                    "Indiana" = 507,
                    "Louisiana" = 1041,
                    "Michigan" = 680,
                    "Minnesota" = 177,
                    "Mississippi" = 508,
                    "Missouri" = 403,
                    "Nevada" = 277,
                    "New Jersey" = 1516,
                    "New York" = 6684,
                    "North Carolina" = 1083,
                    "Ohio" = 775,
                    "Oklahoma" = 213,
                    "South Carolina" = 774,
                    "Tennessee" = 837,
                    "Texas" = 3379,
                    "Virginia" = 777,
                    "Wisconsin" = 184)

ratios.2000 = aids.cases.2000/hiv.cases.2000
ratios.2001 = aids.cases.2001/hiv.cases.2001
ratios.2002 = aids.cases.2002/hiv.cases.2002
ratios.2003 = aids.cases.2003/hiv.cases.2003

df = data.frame(year = rep(c("2000","2001","2002","2003"), each = length(ratios.2000)),
                state = rep(names(aids.cases.2000), 4),
                aids.cases = c(aids.cases.2000,aids.cases.2001,aids.cases.2002,aids.cases.2003),
                hiv.cases = c(hiv.cases.2000,hiv.cases.2001,hiv.cases.2002,hiv.cases.2003),
                ratio = c(ratios.2000,ratios.2001,ratios.2002,ratios.2003))

plot.label = paste0("2000: ",round(mean(df$ratio[df$year=="2000"],na.rm=T),2),
                    ", (sd = ",round(sd(df$ratio[df$year=="2000"], na.rm=T),2),")\n",
                    "2001: ",round(mean(df$ratio[df$year=="2001"],na.rm=T),2),
                    ", (sd = ",round(sd(df$ratio[df$year=="2001"], na.rm=T),2),")\n",
                    "2002: ",round(mean(df$ratio[df$year=="2002"],na.rm=T),2),
                    ", (sd = ",round(sd(df$ratio[df$year=="2002"], na.rm=T),2),")\n",
                    "2003: ",round(mean(df$ratio[df$year=="2003"],na.rm=T),2),
                    ", (sd = ",round(sd(df$ratio[df$year=="2003"], na.rm=T),2),")")

ggplot(df, aes(x=ratio)) + 
  geom_density(mapping = aes(x=ratio,group=year,color=year)) + 
  annotate("label", x = 1.375, y = 2, label = plot.label)

# using all years except 2001 (values seem a bit off)
aids.diagnoses.multiplier = mean(df$ratio[df$year!="2001"],na.rm=T)
aids.diagnoses.sd = sd(df$ratio[df$year!="2001"],na.rm=T)



