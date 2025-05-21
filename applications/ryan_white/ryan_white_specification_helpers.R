

#-- Info on States in Each City --#
# Define Medicaid expansion status and Census region for each state
state_info <- tibble::tribble(
  ~state,              ~medicaid_expansion, ~census_region,
  "Alabama",           "No",                "South",
  "Alaska",            "Yes",               "West",
  "Arizona",           "Yes",               "West",
  "Arkansas",          "Yes",               "South",
  "California",        "Yes",               "West",
  "Colorado",          "Yes",               "West",
  "Connecticut",       "Yes",               "Northeast",
  "Delaware",          "Yes",               "South",
  "District_of_Columbia", "Yes",           "South",
  "Florida",           "No",                "South",
  "Georgia",           "No",                "South",
  "Hawaii",            "Yes",               "West",
  "Idaho",             "Yes",               "West",
  "Illinois",          "Yes",               "Midwest",
  "Indiana",           "Yes",               "Midwest",
  "Iowa",              "Yes",               "Midwest",
  "Kansas",            "No",                "Midwest",
  "Kentucky",          "Yes",               "South",
  "Louisiana",         "Yes",               "South",
  "Maine",             "Yes",               "Northeast",
  "Maryland",          "Yes",               "South",
  "Massachusetts",     "Yes",               "Northeast",
  "Michigan",          "Yes",               "Midwest",
  "Minnesota",         "Yes",               "Midwest",
  "Mississippi",       "No",                "South",
  "Missouri",          "Yes",               "Midwest",
  "Montana",           "Yes",               "West",
  "Nebraska",          "Yes",               "Midwest",
  "Nevada",            "Yes",               "West",
  "New_Hampshire",     "Yes",               "Northeast",
  "New_Jersey",        "Yes",               "Northeast",
  "New_Mexico",        "Yes",               "West",
  "New_York",          "Yes",               "Northeast",
  "North_Carolina",    "Yes",               "South",
  "North_Dakota",      "Yes",               "Midwest",
  "Ohio",              "Yes",               "Midwest",
  "Oklahoma",          "Yes",               "South",
  "Oregon",            "Yes",               "West",
  "Pennsylvania",      "Yes",               "Northeast",
  "Rhode_Island",      "Yes",               "Northeast",
  "South_Carolina",    "No",                "South",
  "South_Dakota",      "Yes",               "Midwest",
  "Tennessee",         "No",                "South",
  "Texas",             "No",                "South",
  "Utah",              "Yes",               "West",
  "Vermont",           "Yes",               "Northeast",
  "Virginia",          "Yes",               "South",
  "Washington",        "Yes",               "West",
  "West_Virginia",     "Yes",               "South",
  "Wisconsin",         "No",                "Midwest",
  "Wyoming",           "No",                "West"
)



STATE.MEDICAID.EXPANSION = state_info$medicaid_expansion=='Yes'
names(STATE.MEDICAID.EXPANSION) = state_info$state

states.name.plus = gsub(" ", "_", get.location.name(locations::get.all.for.type('STATE')))
names(states.name.plus) = locations::get.all.for.type('STATE')
states.name.plus = states.name.plus[sapply(states.name.plus, function(name){
    any(name==names(STATE.MEDICAID.EXPANSION))
})]

MEDICAID.EXPANSION.STATES = names(states.name.plus)[STATE.MEDICAID.EXPANSION[states.name.plus]]
MEDICAID.NONEXPANSION.STATES = names(states.name.plus)[!STATE.MEDICAID.EXPANSION[states.name.plus]]


get.fraction.medicaid.expansion <- function(location)
{
    if (any(MEDICAID.EXPANSION.STATES==location))
        1
    else if (any(MEDICAID.NONEXPANSION.STATES==location))
        0
    else
    {
        name = locations::get.location.name(location)
        
        split = strsplit(name, ', ')
        states.str = split[[1]][length(split[[1]])]
        states = strsplit(states.str, "-")[[1]]
        
        main.state = states[1]
        
        as.numeric(STATE.MEDICAID.EXPANSION[gsub(" ", "_", get.location.name(main.state))])
    }
}
