
# Must call library(locations) prior to sourcing this file

##-------------------------------##
##-- MSAs DEFINED AS CONSTANTS --##
##-------------------------------##

#-- EHE MSAs --#
NYC.MSA = 'C.35620'
MIAMI.MSA = 'C.33100'
LA.MSA = 'C.31080'
ATLANTA.MSA = 'C.12060'

HOUSTON.MSA = 'C.26420'
DALLAS.MSA = 'C.19100'
CHICAGO.MSA = 'C.16980'
DC.MSA = 'C.47900'

PHILADELPHIA.MSA = 'C.37980'
ORLANDO.MSA = 'C.36740'
SF.MSA = 'C.41860'
PHOENIX.MSA = 'C.38060'

TAMPA.MSA = 'C.45300'
RIVERSIDE.MSA = 'C.40140'
DETROIT.MSA = 'C.19820'
BALTIMORE.MSA = 'C.12580'

VEGAS.MSA = 'C.29820'
BOSTON.MSA = 'C.14460'
SAN.DIEGO.MSA = 'C.41740'
CHARLOTTE.MSA = 'C.16740'

SAN.ANTONIO.MSA = 'C.41700'
JACKSONVILLE.MSA = 'C.27260'
NEW.ORLEANS.MSA = 'C.35380'
MEMPHIS.MSA = 'C.32820'

SEATTLE.MSA = 'C.42660'
AUSTIN.MSA = 'C.12420'
CINCINATTI.MSA = 'C.17140'

COLUMBUS.MSA = 'C.18140'
BATON.ROUGE.MSA = 'C.12940'
SACRAMENTO.MSA = 'C.40900'

#Adding SHIELD specific MSAs:
DENVER.MSA = "C.19740"
PORTLAND.MSA = "C.38900" 
KANSAS.CITY.MSA ="C.28140"
ST.LOUIS.MSA = "C.41180"
MINNEAPOLIS.MSA = "C.33460" 
OKLAHOMA.CITY.MSA = "C.36420" 
ALBUQUERQUE.MSA = "C.10740"
MILWAUKEE.MSA = "C.33340" 
BIRMINGHAM.MSA = "C.13820"
ROCHESTER.MSA = "C.40380"


##----------------------------##
##-- LUMP THEM INTO VECTORS --##
##----------------------------##

# Our set of typically used MSAs for SHIELD.
SHIELD.MSAS.OF.INTEREST = c(NYC=NYC.MSA,
                            Miami=MIAMI.MSA,
                            LA=LA.MSA,
                            Atlanta=ATLANTA.MSA,
                            Houston=HOUSTON.MSA,
                            Dallas=DALLAS.MSA,
                            Chicago=CHICAGO.MSA,
                            DC=DC.MSA,
                            Philadelphia=PHILADELPHIA.MSA,
                            Orlando=ORLANDO.MSA,
                            SF=SF.MSA,
                            Phoenix=PHOENIX.MSA,
                            Tampa=TAMPA.MSA,
                            Riverside=RIVERSIDE.MSA,
                            Detroit=DETROIT.MSA,
                            Baltimore=BALTIMORE.MSA,
                            Vegas=VEGAS.MSA,
                            Boston=BOSTON.MSA,
                            San_Diego=SAN.DIEGO.MSA,
                            Charlotte=CHARLOTTE.MSA,
                            San_Antonio=SAN.ANTONIO.MSA,
                            Jacksonville=JACKSONVILLE.MSA,
                            New_Orleans=NEW.ORLEANS.MSA,
                            Memphis=MEMPHIS.MSA,
                            Seattle=SEATTLE.MSA,
                            Austin=AUSTIN.MSA,
                            Cincinatti=CINCINATTI.MSA,
                            Columbus=COLUMBUS.MSA,
                            Baton_Rouge=BATON.ROUGE.MSA,
                            Sacramento=SACRAMENTO.MSA,
                            Denver = DENVER.MSA,
                            Portland = PORTLAND.MSA,
                            Kansas_City = KANSAS.CITY.MSA,
                            St_Louis = ST.LOUIS.MSA,
                            Minneapolis = MINNEAPOLIS.MSA,
                            Oklahoma_City = OKLAHOMA.CITY.MSA,
                            Albuquerque = ALBUQUERQUE.MSA,
                            Milwaukee = MILWAUKEE.MSA,
                            Birmingham = BIRMINGHAM.MSA,
                            Rochester = ROCHESTER.MSA)

