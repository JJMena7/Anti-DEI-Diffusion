#################################################################
"Anti-DEI Project Analysis Introduction and Adoption Wrangling" 
#################################################################
#Date: 4.27.26

# Basic setup
knitr::opts_chunk$set(
  echo = TRUE,
  warning = FALSE,
  message = FALSE,
  dpi = 300,
  dev = 'png'
)
options(scipen = 999)

# set working Directory 
setwd("/Users/jjmena7/Desktop/diss_r_analysis")

#Load Packages
library(readxl)
library(tidyverse)
library(tidycensus)
library(janitor)
library(dplyr)
library(writexl)
#install.packages("writexl")

#load data
dei_df <- read_excel("anti_dei_panel_data.xlsx", sheet = 3) |>
  clean_names() |>
  select(
    state_id,
    state,
    state_abbr,
    year,
    intro_any,
    intro_count,
    adopt_any,
    rep_leg_prop_raw,
    rep_leg_prop_lag,
    state_white_prop_raw
  )

# 2. Check structure
glimpse(dei_df)

# 4. Create lagged variables within state
dei_lagged <- dei_df |>
  group_by(state) |>
  arrange(year, .by_group = TRUE) %>%
  mutate(
    rep_leg_prop_lag = lag(rep_leg_prop_raw, 1)
  ) %>%
  ungroup()

#Save as new excel sheet file
# 5. Save as a new Excel file
#write_xlsx(
#  dei_lagged,
#  "/Users/jjmena7/Desktop/diss_r_analysis/anti_dei_panel_data_4.27.26.xlsx"
#)




#################PRACTICE ACS DATA EXTRACTION CODE#######################
#Extracting State White proportion from Census
#---------------------------------------------#

#input census API key
library(tidycensus)
library(tidyverse)

census_api_key("30013846e58da2cd23693309dc3b01f72a687889", install = TRUE)


#Search for variables
#SEARCHING FOR VARIABLES

acs5_look_up_2020 <- load_variables(2020, "acs5", cache = TRUE)
view(acs5_look_up_2020)


# Function to pull one ACS year
#Pulling ACS5 data since ACS1 is unavilable for 2021
#Note to self: will try to pull ACS1 data for 2021 - 2025 later
#Pulled B03002_001 (Total population) & B03002_003 (Not Hispanic or Latino: White alone)

years <- 2020:2025

get_state_white_prop <- function(year) {
  
  message("Trying ACS 5-year year: ", year)
  
  get_acs(
    geography = "state",
    survey = "acs5",
    year = year,
    variables = c(
      total_pop = "B03002_001",
      white_nh  = "B03002_003"
    ),
    output = "wide"
  ) %>%
    transmute(
      acs_year = year,
      acs_period = paste0(year - 4, "-", year),
      GEOID,
      state = NAME,
      total_pop = total_popE,
      white_nh = white_nhE,
      white_nh_prop = white_nh / total_pop,
      white_nh_pct = 100 * white_nh_prop,
      total_pop_moe = total_popM,
      white_nh_moe = white_nhM
    )
}

safe_get_state_white_prop <- possibly(
  .f = get_state_white_prop,
  otherwise = NULL
)

state_white_acs5 <- map_dfr(years, safe_get_state_white_prop)


state_white_acs5


#arrange and select data into long form
state_white_export <- state_white_acs5 %>%
  select(
    state,
    acs_year,
    white_nh_prop,
    white_nh_pct
  ) %>%
  arrange(state, acs_year)

#View data
view(state_white_export)

#export to csv
write_csv(
  state_white_export,
  "state_white_nonhispanic_proportions_acs5_2020_2024.csv"
)


#Extract for ACS1 data 2020 - 2025












#################ACS Year 1 & 5 Estimates#################################
#state residents ages 15–24 who identify as Non-Hispanic White alone

#variables: 
#B01001H: Sex by age for White alone, not Hispanic or Latino
#B01001: Sex by age for total population



#Non-Hispanic White alone ages 15–24 variable
white_nh_age_vars <- c(
  "B01001H_006", #Male 15-17
  "B01001H_007", #Male 18-19
  "B01001H_008", #Male 20-24
  
  "B01001H_021", #Female 15-17
  "B01001H_022", #Female 18-19
  "B01001H_023"  #Female 20-24
)

#Total age variable 
total_age_vars <- c(
  "B01001_006", # Male 15-17
  "B01001_007", # Male 18-19
  "B01001_008", # Male 20
  "B01001_009", # Male 21
  "B01001_010", # Male 22-24
  
  "B01001_030", # Female 15-17
  "B01001_031", # Female 18-19
  "B01001_032", # Female 20
  "B01001_033", # Female 21
  "B01001_034"  # Female 22-24
)


#proportion of Non-Hispanic White alone ages 15–24 would be: 

#white_nh_age_vars / total_age_vars


#Pulling ACS 5 data 

years<- 2020:2024

get_state_white_nh_15_24 <- function(year) {
  
  message("Getting data from the ", year - 4, "-", year, " 5-year ACS")
  
  tidycensus::get_acs(
    geography = "state",
    survey = "acs5",
    year = year,
    variables = c(
      # Numerator: Non-Hispanic White alone ages 15-24
      white_nh_m_15_17 = "B01001H_006",
      white_nh_m_18_19 = "B01001H_007",
      white_nh_m_20_24 = "B01001H_008",
      white_nh_f_15_17 = "B01001H_021",
      white_nh_f_18_19 = "B01001H_022",
      white_nh_f_20_24 = "B01001H_023",
      
      # Denominator: Total population ages 15-24
      total_m_15_17 = "B01001_006",
      total_m_18_19 = "B01001_007",
      total_m_20    = "B01001_008",
      total_m_21    = "B01001_009",
      total_m_22_24 = "B01001_010",
      total_f_15_17 = "B01001_030",
      total_f_18_19 = "B01001_031",
      total_f_20    = "B01001_032",
      total_f_21    = "B01001_033",
      total_f_22_24 = "B01001_034"
    ),
    output = "wide",
    cache_table = FALSE
  ) %>%
    transmute(
      year = year,
      acs_period = paste0(year - 4, "-", year),
      GEOID,
      state = NAME,
      
      white_nh_15_24 =
        white_nh_m_15_17E +
        white_nh_m_18_19E +
        white_nh_m_20_24E +
        white_nh_f_15_17E +
        white_nh_f_18_19E +
        white_nh_f_20_24E,
      
      total_15_24 =
        total_m_15_17E +
        total_m_18_19E +
        total_m_20E +
        total_m_21E +
        total_m_22_24E +
        total_f_15_17E +
        total_f_18_19E +
        total_f_20E +
        total_f_21E +
        total_f_22_24E,
      
      white_nh_15_24_prop = white_nh_15_24 / total_15_24,
      white_nh_15_24_pct = 100 * white_nh_15_24_prop
    )
}

state_white_nh_15_24 <- map_dfr(years, get_state_white_nh_15_24)


#View
view(state_white_nh_15_24)

#arrange data by state and year & round third decimal
state_white_nh_15_24_export <- state_white_nh_15_24 %>%
  transmute(
    state,
    year,
    acs_period,
    white_nh_15_24_prop = round(white_nh_15_24_prop, 3),
    white_nh_15_24_pct = round(white_nh_15_24_pct, 3)
  ) %>%
  arrange(state, year)


#view data
view(state_white_nh_15_24_export)

#check data 
state_white_nh_15_24_export %>%
  filter(state == "Alabama")

#export dataset file for ACS5 estimates (2020 - 2024)
write_csv(
  state_white_nh_15_24_export,
  "state_white_nh_15_24_proportions_acs5_2020_2024.csv"
)

#ACS1 data extract------

year_acs1 <- 2021:2024

get_state_white_nh_15_24_acs1 <- function(year) {
  
  message("Getting data from the ", year, " 1-year ACS")
  
  tidycensus::get_acs(
    geography = "state",
    survey = "acs1",
    year = year,
    variables = c(
      # Numerator: Non-Hispanic White alone ages 15-24
      white_nh_m_15_17 = "B01001H_006",
      white_nh_m_18_19 = "B01001H_007",
      white_nh_m_20_24 = "B01001H_008",
      white_nh_f_15_17 = "B01001H_021",
      white_nh_f_18_19 = "B01001H_022",
      white_nh_f_20_24 = "B01001H_023",
      
      # Denominator: Total population ages 15-24
      total_m_15_17 = "B01001_006",
      total_m_18_19 = "B01001_007",
      total_m_20    = "B01001_008",
      total_m_21    = "B01001_009",
      total_m_22_24 = "B01001_010",
      total_f_15_17 = "B01001_030",
      total_f_18_19 = "B01001_031",
      total_f_20    = "B01001_032",
      total_f_21    = "B01001_033",
      total_f_22_24 = "B01001_034"
    ),
    output = "wide",
    cache_table = FALSE
  ) %>%
    transmute(
      year = year,
      GEOID,
      state = NAME,
      
      white_nh_15_24 =
        white_nh_m_15_17E +
        white_nh_m_18_19E +
        white_nh_m_20_24E +
        white_nh_f_15_17E +
        white_nh_f_18_19E +
        white_nh_f_20_24E,
      
      total_15_24 =
        total_m_15_17E +
        total_m_18_19E +
        total_m_20E +
        total_m_21E +
        total_m_22_24E +
        total_f_15_17E +
        total_f_18_19E +
        total_f_20E +
        total_f_21E +
        total_f_22_24E,
      
      white_nh_15_24_prop = white_nh_15_24 / total_15_24,
      white_nh_15_24_pct = 100 * white_nh_15_24_prop
    )
}

state_white_nh_15_24_acs1 <- map_dfr(
  year_acs1,
  get_state_white_nh_15_24_acs1
)


#view data
view(state_white_nh_15_24_acs1)

#ACS 1arrange data by state and year & round third decimal
state_white_nh_15_24_acs1_export <- state_white_nh_15_24_acs1 %>%
  transmute(
    state,
    year,
    white_nh_15_24_prop_ac1 = round(white_nh_15_24_prop, 3),
    white_nh_15_24_pct_ac1 = round(white_nh_15_24_pct, 3)
  ) %>%
  arrange(state, year)


view(state_white_nh_15_24_acs1_export)


#export dataset file for ACS1 estimates (2021 - 2024)
write_csv(
  state_white_nh_15_24_acs1_export,
  "state_white_nh_15_24_proportions_acs1_2020_2024.csv"
)

#data notes AC1 data is missing 2020 year data, however AC5 data has 

#ac1 white proportions 15 - 24 data (2021-2024) (4)
view(state_white_nh_15_24_acs1_export)

#ac5 white proportions 15 - 24 data (2020-2024) (5)
view(state_white_nh_15_24_export)



###merge both ACS 1 and ACS 5 datasets

# Add ACS source label to ACS 1-year file
acs1_export <- state_white_nh_15_24_acs1_export %>%
  mutate(acs_type = "ACS1")

# Add ACS source label to ACS 5-year file
acs5_export <- state_white_nh_15_24_export %>%
  mutate(acs_type = "ACS5")

# Combine into one long-format dataset
state_white_nh_15_24_combined <- bind_rows(
  acs1_export,
  acs5_export
) %>%
  arrange(state, acs_type, year)

# View combined file
View(state_white_nh_15_24_combined)

#######Creating State Flagship White Estimates###############

#Create data frames

#institutions
Institution <- c("University of Alabama, Tuscaloosa",
                 "University of Alaska Fairbanks",
                 "University of Arizona", 
                 "University of Arkansas Fayetteville", 
                 "University of California, Berkeley", 
                 "University of Colorado Boulder", 
                 "University of Connecticut, Storrs", 
                 "University of Delaware", 
                 "University of Florida",
                 "University of Georgia",
                 "University of Hawaiʻi at Mānoa",
                 "University of Idaho", 
                 "University of Illinois at Urbana-Champaign",
                 "Indiana University Bloomington",
                 "The University of Iowa",
                 "University of Kansas, Lawrence",
                 "University of Kentucky, Lexington",
                 "Louisiana State University, Baton Rouge",
                 "The University of Maine, Orono",
                 "University of Maryland, College Park",
                 "University of Massachusetts Amherst", 
                 "University of Michigan, Ann Arbor",
                 "University of Minnesota, Twin Cities",
                 "University of Mississippi",
                 "University of Missouri, Columbia",
                 "University of Montana, Missoula",
                 "University of Nebraska–Lincoln",
                 "University of Nevada, Reno",
                 "University of New Hampshire, Durham", 
                 "Rutgers University–New Brunswick",
                 "The University of New Mexico, Albuquerque",
                 "University at Buffalo (SUNY)",
                 "The University of North Carolina at Chapel Hill",
                 "University of North Dakota, Grand Forks", 
                 "The Ohio State University, Columbus",
                 "The University of Oklahoma, Norman Campus",
                 "University of Oregon, Eugene",
                 "Pennsylvania State University",
                 "The University of Rhode Island",
                 "University of South Carolina, Columbia",
                 "University of South Dakota, Vermillion",
                 "The University of Tennessee, Knoxville",
                 "The University of Texas at Austin",
                 "The University of Utah",
                 "The University of Vermont",
                 "University of Virginia, Charlottesville",
                 "University of Washington",
                 "West Virginia University, Morgantown",
                 "University of Wisconsin–Madison",
                 "University of Wyoming" )
             
State <- c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut",
                       "Delaware","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas",
                       "Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota",
                       "Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey",
                       "New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon",
                       "Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas",
                       "Utah","Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming")

Years <- c("2020","2021","2022", "2023", "2024", "2025")

#create data set by institution for each year
state_flag_white <- data.frame(
  Institution = rep(Institution, each = length(Years)),
  State = rep(State, each = length(Years)),
  Years = rep(Years, times = length(Institution))
)


head(state_flag_white)

#create new excel file
write_xlsx(state_flag_white, "/Users/jjmena7/Desktop/diss_r_analysis/state_flagship_white.xlsx")







