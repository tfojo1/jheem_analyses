
# Must call library(locations) prior to sourcing this file

##-------------------------------##
##-- MSAs DEFINED AS CONSTANTS --##
##-------------------------------##

#-- EHE MSAs --#
NYC.MSA = 'c.35620'
MIAMI.MSA = 'c.33100'
LA.MSA = 'c.31080'
ATLANTA.MSA = 'c.12060'

HOUSTON.MSA = 'c.26420'
DALLAS.MSA = 'c.19100'
CHICAGO.MSA = 'c.16980'
DC.MSA = 'c.47900'

PHILADELPHIA.MSA = 'c.37980'
ORLANDO.MSA = 'c.36740'
SF.MSA = 'c.41860'
PHOENIX.MSA = 'c.38060'

TAMPA.MSA = 'c.45300'
RIVERSIDE.MSA = 'c.40140'
DETROIT.MSA = 'c.19820'
BALTIMORE.MSA = 'c.12580'

VEGAS.MSA = 'c.29820'
BOSTON.MSA = 'c.14460'
SAN.DIEGO.MSA = 'c.41740'
CHARLOTTE.MSA = 'c.16740'

SAN.ANTONIO.MSA = 'c.41700'
JACKSONVILLE.MSA = 'c.27260'
NEW.ORLEANS.MSA = 'c.35380'
MEMPHIS.MSA = 'c.32820'

SEATTLE.MSA = 'c.42660'
AUSTIN.MSA = 'c.12420'
INDIANAPOLIS.MSA = 'c.26900'
CINCINATTI.MSA = 'c.17140'

COLUMBUS.MSA = 'c.18140'
BATON.ROUGE.MSA = 'c.12940'
SACRAMENTO.MSA = 'c.40900'
CLEVELAND.MSA = 'c.17460'

#-- Non-EHE MSAs --#
ST.LOUIS.MSA = 'c.41180'
DENVER.MSA = 'c.19740'
PORTLAND.MSA = 'c.38900'

BIRMINGHAM.MSA = 'c.13820'
MOBILE.MSA = 'c.33660'
JACKSON.MSA = 'c.27140'

##----------------------------##
##-- LUMP THEM INTO VECTORS --##
##----------------------------##

# Every MSA we have represented in the code anywhere
MSAS.OF.INTEREST = c(NYC=NYC.MSA,
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
                     Indianapolis=INDIANAPOLIS.MSA,
                     Cincinatti=CINCINATTI.MSA,
                     Columbus=COLUMBUS.MSA,
                     Baton_Rouge=BATON.ROUGE.MSA,
                     Sacramento=SACRAMENTO.MSA,
                     Cleveland=CLEVELAND.MSA,
                     St_Louis=ST.LOUIS.MSA)

# Just the ones for EHE counties
EHE.MSAS = MSAS.OF.INTEREST[1:32]


##-- LIST OF EVERY LOCATION WE CARE ABOUT --#
LOCATIONS.OF.INTEREST = MSAS.OF.INTEREST
COUNTIES.OF.INTEREST = get.sub.locations(LOCATIONS.OF.INTEREST, 'county', T)