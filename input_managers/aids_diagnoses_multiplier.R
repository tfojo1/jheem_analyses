# Using data from 2000, 2002, and 2003 (2001 values seem off), 
# calculating the ratio of AIDS diagnoses:HIV diagnoses in states that have at least 100 HIV diagnoses 

# only including states with >=100 cases 

# OLD: 
# aids.diagnoses.multiplier
    # 1.044985
# aids.diagnoses.sd 
    # 0.2306945

library(ggplot2)

# vol 9-2, Table 27
hiv.cases.1997 = c("Alabama" = 529,
                   "Arizona" = 343,
                   "Arkansas" = 208,
                   "Colorado" = 358,
                   "Florida" = 1948,
                   "Indiana" = 484,
                   "Louisiana" = 1091,
                   "Michigan" = 820,
                   "Minnesota" = 221,
                   "Mississippi" = 529,
                   "Missouri" = 494,
                   "Nevada" = 356,
                   "New Jersey" = 1940,
                   "New York" = NA,
                   "North Carolina" = 1111,
                   "Ohio" = 447,
                   "Oklahoma" = 219,
                   "South Carolina" = 718,
                   "Tennessee" = 866,
                   "Texas" = NA,
                   "Virginia" = 930,
                   "Wisconsin" = 224)
# vol 9-2, Table 1
aids.cases.1997 = c("Alabama" = 570,
                    "Arizona" = 448,
                    "Arkansas" = 242,
                    "Colorado" = 380,
                    "Florida" = 6098,
                    "Indiana" = 523,
                    "Louisiana" = 1094,
                    "Michigan" = 882,
                    "Minnesota" = 214,
                    "Mississippi" = 347,
                    "Missouri" = 577,
                    "Nevada" = 592,
                    "New Jersey" = 3226,
                    "New York" = NA,
                    "North Carolina" = 850,
                    "Ohio" = 848,
                    "Oklahoma" = 283,
                    "South Carolina" = 779,
                    "Tennessee" = 784,
                    "Texas" = 4718,
                    "Virginia" = 1175,
                    "Wisconsin" = 255)

# vol 10-2, Table 3
hiv.cases.1998 = c("Alabama" = 521,
                   "Arizona" = 473,
                   "Arkansas" = 240,
                   "Colorado" = 285,
                   "Florida" = 6837,
                   "Indiana" = 366,
                   "Louisiana" = 1030,
                   "Michigan" = 584,
                   "Minnesota" = 238,
                   "Mississippi" = 507,
                   "Missouri" = 465,
                   "Nevada" = 260,
                   "New Jersey" = 1473,
                   "New York" = NA,
                   "North Carolina" = 1021,
                   "Ohio" = 906,
                   "Oklahoma" = 280,
                   "South Carolina" = 899,
                   "Tennessee" = 756,
                   "Texas" = NA,
                   "Virginia" = 810,
                   "Wisconsin" = 199)

# vol 10-2, Table 2
aids.cases.1998 = c("Alabama" = 484,
                    "Arizona" = 645,
                    "Arkansas" = 203,
                    "Colorado" =314,
                    "Florida" = 5448,
                    "Indiana" = 484,
                    "Louisiana" = 951,
                    "Michigan" = 714,
                    "Minnesota" = 190,
                    "Mississippi" = 415,
                    "Missouri" = 443,
                    "Nevada" = 258,
                    "New Jersey" = 2134,
                    "New York" = NA,
                    "North Carolina" = 788,
                    "Ohio" = 685,
                    "Oklahoma" = 285,
                    "South Carolina" = 777,
                    "Tennessee" = 695,
                    "Texas" = 3967,
                    "Virginia" = 998,
                    "Wisconsin" = 203)

# vol 11-2, Table 3
hiv.cases.1999 = c("Alabama" = 519,
                   "Arizona" = 694,
                   "Arkansas" = 214,
                   "Colorado" = 325,
                   "Florida" = 6402,
                   "Indiana" = 301,
                   "Louisiana" = 971,
                   "Michigan" = 499,
                   "Minnesota" = 230,
                   "Mississippi" = 464,
                   "Missouri" = 472,
                   "Nevada" = 232,
                   "New Jersey" = 1330,
                   "New York" = NA,
                   "North Carolina" = 1017,
                   "Ohio" = 919,
                   "Oklahoma" = 219,
                   "South Carolina" = 717,
                   "Tennessee" = 897,
                   "Texas" = 2563,
                   "Virginia" = 878,
                   "Wisconsin" = 184)

# vol 11-2, Table 2
aids.cases.1999 = c("Alabama" = 476,
                    "Arizona" = 880,
                    "Arkansas" = 194,
                    "Colorado" = 319,
                    "Florida" = 5468,
                    "Indiana" = 363,
                    "Louisiana" = 854,
                    "Michigan" = 649,
                    "Minnesota" = 190,
                    "Mississippi" = 421,
                    "Missouri" = 531,
                    "Nevada" = 242,
                    "New Jersey" = 2043,
                    "New York" = NA,
                    "North Carolina" = 794,
                    "Ohio" = 547,
                    "Oklahoma" = 148,
                    "South Carolina" = 959,
                    "Tennessee" = 759,
                    "Texas" = 3181,
                    "Virginia" = 943,
                    "Wisconsin" = 152)

# vol 12-2, Table 3
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

# vol 12-2, Table 2
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

# vol 13-2, Table 3
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

# vol 13-2, Table 2
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

# vol 14-2, Table 15
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

# vol 14-2, Table 14
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

# vol 15, Table 16 
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

# vol 15, Table 14
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

# vol 16, Table 16 
hiv.cases.2004 = c("Alabama" = 488,
                   "Arizona" = 508,
                   "Arkansas" = 163,
                   "Colorado" = 371,
                   "Florida" = 5107,
                   "Indiana" = 372,
                   "Louisiana" = 902,
                   "Michigan" = 602,
                   "Minnesota" = 225,
                   "Mississippi" = 340,
                   "Missouri" = 408,
                   "Nevada" = 313,
                   "New Jersey" = 1704,
                   "New York" = 6033,
                   "North Carolina" = 1099,
                   "Ohio" = 800,
                   "Oklahoma" = 125,
                   "South Carolina" = 571,
                   "Tennessee" = 780,
                   "Texas" = 4143,
                   "Virginia" = 858,
                   "Wisconsin" = 180)

# vol 16, Table 14
aids.cases.2004 = c("Alabama" = 466,
                    "Arizona" = 563,
                    "Arkansas" = 185,
                    "Colorado" = 338,
                    "Florida" = 5822,
                    "Indiana" = 396,
                    "Louisiana" = 1010,
                    "Michigan" = 655,
                    "Minnesota" = 218,
                    "Mississippi" = 479,
                    "Missouri" = 394,
                    "Nevada" = 305,
                    "New Jersey" = 1848,
                    "New York" = 7641,
                    "North Carolina" = 1137,
                    "Ohio" = 665,
                    "Oklahoma" = 195,
                    "South Carolina" = 759,
                    "Tennessee" = 774,
                    "Texas" = 3298,
                    "Virginia" = 796,
                    "Wisconsin" = 177)

# vol 17, Table 16 
hiv.cases.2005 = c("Alabama" = 468,
                   "Arizona" = 709,
                   "Arkansas" = 253,
                   "Colorado" = 371,
                   "Florida" = 4637,
                   "Indiana" = 351,
                   "Louisiana" = 695,
                   "Michigan" = 655,
                   "Minnesota" = 257,
                   "Mississippi" = 399,
                   "Missouri" = 469,
                   "Nevada" = 287,
                   "New Jersey" = 1247,
                   "New York" = 5509,
                   "North Carolina" = 1140,
                   "Ohio" = 949,
                   "Oklahoma" = 248,
                   "South Carolina" = 527,
                   "Tennessee" = 702,
                   "Texas" = 3682,
                   "Virginia" = 796,
                   "Wisconsin" = 112)

# vol 17, Table 14
aids.cases.2005 = c("Alabama" = 518,
                    "Arizona" = 642,
                    "Arkansas" = 242,
                    "Colorado" = 359,
                    "Florida" = 4960,
                    "Indiana" = 409,
                    "Louisiana" = 961,
                    "Michigan" = 822,
                    "Minnesota" = 225,
                    "Mississippi" = 387,
                    "Missouri" = 386,
                    "Nevada" = 296,
                    "New Jersey" = 1278,
                    "New York" = 6299,
                    "North Carolina" = 945,
                    "Ohio" = 784,
                    "Oklahoma" = 282,
                    "South Carolina" = 668,
                    "Tennessee" = 841,
                    "Texas" = 3113,
                    "Virginia" = 646,
                    "Wisconsin" = 123)

# vol 18, Table 16 
hiv.cases.2006 = c("Alabama" = 502,
                   "Arizona" = 617,
                   "Arkansas" = 206,
                   "Colorado" = 319,
                   "Florida" = 4163,
                   "Indiana" = 399,
                   "Louisiana" = 728,
                   "Michigan" = 635,
                   "Minnesota" = 309,
                   "Mississippi" = 352,
                   "Missouri" = 502,
                   "Nevada" = 339,
                   "New Jersey" = 1477,
                   "New York" = 5326,
                   "North Carolina" = 1615,
                   "Ohio" = 867,
                   "Oklahoma" = 229,
                   "South Carolina" = 493,
                   "Tennessee" = 732,
                   "Texas" = 3370,
                   "Virginia" = 864,
                   "Wisconsin" = 261)

# vol 18, Table 14
aids.cases.2006 = c("Alabama" = 462,
                    "Arizona" = 539,
                    "Arkansas" = 252,
                    "Colorado" = 322,
                    "Florida" = 4932,
                    "Indiana" = 346,
                    "Louisiana" = 824,
                    "Michigan" = 672,
                    "Minnesota" = 212,
                    "Mississippi" = 365,
                    "Missouri" = 469,
                    "Nevada" = 294,
                    "New Jersey" = 1065,
                    "New York" = 5495,
                    "North Carolina" = 1229,
                    "Ohio" = 767,
                    "Oklahoma" = 205,
                    "South Carolina" = 705,
                    "Tennessee" = 680,
                    "Texas" = 2998,
                    "Virginia" = 605,
                    "Wisconsin" = 217)

# vol 19, Table 18 (using reported, not diagnosed)
hiv.cases.2007 = c("Alabama" = 529,
                   "Arizona" = 771,
                   "Arkansas" = 206,
                   "Colorado" = 382,
                   "Florida" = 5165,
                   "Indiana" = 406,
                   "Louisiana" = 797,
                   "Michigan" = 623,
                   "Minnesota" = 289,
                   "Mississippi" = 471,
                   "Missouri" = 460,
                   "Nevada" = 369,
                   "New Jersey" = 1571,
                   "New York" = 5197,
                   "North Carolina" = 1746,
                   "Ohio" = 852,
                   "Oklahoma" = 199,
                   "South Carolina" = 542,
                   "Tennessee" = 841,
                   "Texas" = 3495,
                   "Virginia" = 823,
                   "Wisconsin" = 220)

# vol 19, Table 16
aids.cases.2007 = c("Alabama" = 391,
                    "Arizona" = 585,
                    "Arkansas" = 196,
                    "Colorado" = 355,
                    "Florida" = 3961,
                    "Indiana" = 329,
                    "Louisiana" = 879,
                    "Michigan" = 628,
                    "Minnesota" = 197,
                    "Mississippi" = 352,
                    "Missouri" = 542,
                    "Nevada" = 335,
                    "New Jersey" = 1164,
                    "New York" = 4810,
                    "North Carolina" = 1024,
                    "Ohio" = 703,
                    "Oklahoma" = 264,
                    "South Carolina" = 742,
                    "Tennessee" = 658,
                    "Texas" = 2964,
                    "Virginia" = 634,
                    "Wisconsin" = 199)

ratios.1997 = aids.cases.1997/hiv.cases.1997
ratios.1998 = aids.cases.1998/hiv.cases.1998
ratios.1999 = aids.cases.1999/hiv.cases.1999
ratios.2000 = aids.cases.2000/hiv.cases.2000
ratios.2001 = aids.cases.2001/hiv.cases.2001
ratios.2002 = aids.cases.2002/hiv.cases.2002
ratios.2003 = aids.cases.2003/hiv.cases.2003
ratios.2004 = aids.cases.2004/hiv.cases.2004
ratios.2005 = aids.cases.2005/hiv.cases.2005
ratios.2006 = aids.cases.2006/hiv.cases.2006
ratios.2007 = aids.cases.2007/hiv.cases.2007

sd.1997 = sd(ratios.1997, na.rm = T)
sd.1998 = sd(ratios.1998, na.rm = T)
sd.1999 = sd(ratios.1999, na.rm = T)
sd.2000 = sd(ratios.2000, na.rm = T)
sd.2001 = sd(ratios.2001, na.rm = T)
sd.2002 = sd(ratios.2002, na.rm = T)
sd.2003 = sd(ratios.2003, na.rm = T)
sd.2004 = sd(ratios.2004, na.rm = T)
sd.2005 = sd(ratios.2005, na.rm = T)
sd.2006 = sd(ratios.2006, na.rm = T)
sd.2007 = sd(ratios.2007, na.rm = T)

ratios = c(ratios.1997,ratios.1998,ratios.1999,
           ratios.2000,ratios.2001,ratios.2002,
           ratios.2003,ratios.2004,ratios.2005,
           ratios.2006,ratios.2007)

dim.names = list(state = names(ratios.1997),
                 year = as.character(c(1997:2007)))

ratios.array = array(ratios,
                     dim = sapply(dim.names, length),
                     dimnames = dim.names)
                       
sds.array = c(sd.1997,sd.1998,sd.1999,
              sd.2000,sd.2001,sd.2002,
              sd.2003,sd.2004,sd.2005,
              sd.2006,sd.2007)

names(sds.array) = as.character(c(1997:2007))

df = data.frame(year = rep(c(as.character(c(1997:2007))), each = length(ratios.2000)),
                state = rep(names(aids.cases.2000), 11),
                ratio = ratios,
                sd = sds)

aids.diagnoses.multipliers = apply(ratios.array,"year",mean, na.rm = T)
aids.diagnoses.sds = sds.array

plot.label = paste(c(1997:2007), ":", round(aids.diagnoses.multipliers,2),
                    " (sd = ",round(aids.diagnoses.sds,2),")\n", collapse = " ")

ggplot(df, aes(x=ratio)) + 
  geom_density(mapping = aes(x=ratio,group=year,color=year)) + 
  annotate("label", x = 2.5, y = 2, label = plot.label)

# using all years except 2001 (values seem a bit off)
# aids.diagnoses.multiplier = mean(df$ratio[df$year!="2001"],na.rm=T)
# aids.diagnoses.sd = sd(df$ratio[df$year!="2001"],na.rm=T)


aids.diagnoses.multipliers
aids.diagnoses.sds
