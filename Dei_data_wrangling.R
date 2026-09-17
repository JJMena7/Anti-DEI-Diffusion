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
    governor_party,
    rep_leg_prop,
    rep_leg_prop_lag,
    state_white_prop_acs5,
    state_white_prop_acs1,
    flagship_white_prop
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

#census_api_key("30013846e58da2cd23693309dc3b01f72a687889", install = TRUE)


#Search for variables
#SEARCHING FOR VARIABLES

acs5_look_up_2020 <- load_variables(2020, "acs5", cache = TRUE)
view(acs5_look_up_2020)


# Function to pull one ACS year
#Pulling ACS5 data since ACS1 is unavilable for 2021
#Note to self: will try to pull ACS1 data for 2021 - 2025 later
#Pulled B03002_001 (Total population) & B03002_003 (Not Hispanic or Latino: White alone)

#years <- 2020:2025

years <- 2019:2025

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

#data from 2019 - 20245 for ACS5 data
#export to csv
write_csv(
  state_white_export,
  "state_white_nonhispanic_proportions_acs5_2019_2024.csv"
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


#Pulling ACS 5 data (pulling 18 - 24 data )

years<- 2018:2024

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

#arrange data by state and year 
state_white_nh_15_24_export <- state_white_nh_15_24 %>%
  transmute(
    state,
    year,
    acs_period,
    white_nh_15_24_prop,
    white_nh_15_24_pct) %>% arrange(state, year)


#view data
view(state_white_nh_15_24_export)

#check data 
state_white_nh_15_24_export %>%
  filter(state == "Alabama")

#export dataset file for ACS5 estimates (2019 - 2024)
write_csv(
  state_white_nh_15_24_export,
  "state_white_nh_15_24_proportions_acs5_2018_2024.csv"
)

#ACS1 data extract for 2021 and 2024

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

state_white_nh_15_24_acs1 <- map_dfr(year_acs1, get_state_white_nh_15_24_acs1)


#view data
view(state_white_nh_15_24_acs1)

#ACS 1arrange data by state and year 
state_white_nh_15_24_acs1_export <- state_white_nh_15_24_acs1 %>%
  transmute(
    state,
    year,
    white_nh_15_24_prop,
    white_nh_15_24_pct
  ) %>%
  arrange(state, year)


view(state_white_nh_15_24_acs1_export)



#Acs1 2018 data

year_2018 <- 2018

get_state_white_acs1_2018 <- function(year) {
  
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

state_white_nh_acs1_18 <- map_dfr(year_2018, get_state_white_acs1_2018)

view(state_white_nh_acs1_18)

#arrange data 

state_white_nh_acs1_18 <- state_white_nh_acs1_18 |>
  transmute(
    state,
    year,
    white_nh_15_24_prop,
    white_nh_15_24_pct
  ) %>%
  arrange(state, year)


view(state_white_nh_acs1_18)


#Acs1 2019 data 

year_2019 <- 2019

get_state_white_acs1_2019 <- function(year) {
  
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

state_white_nh_acs1_19 <- map_dfr(year_2019, get_state_white_acs1_2019)

view(state_white_nh_acs1_19)

#arrange data 

state_white_nh_acs1_19 <- state_white_nh_acs1_19 |>
  transmute(
    state,
    year,
    white_nh_15_24_prop,
    white_nh_15_24_pct
  ) %>%
  arrange(state, year)


view(state_white_nh_acs1_19)


#2020 AC1 experimental estimates

library(tidyverse)
library(tidycensus)

# ------------------------------------------------------------
# Setup
# ------------------------------------------------------------

state_abbrs <- c(state.abb)

pums_base_url <- paste0(
  "https://www2.census.gov/programs-surveys/acs/",
  "experimental/2020/data/pums/1-Year/"
)

# ------------------------------------------------------------
# Function
# ------------------------------------------------------------

get_state_white_nh_15_24_2020 <- function(state_abbr) {

  message("Processing ", state_abbr)

  zip_name <- paste0(
    "csv_p",
    tolower(state_abbr),
    ".zip"
  )

  zip_url <- paste0(
    pums_base_url,
    zip_name
  )

  zip_path <- tempfile(fileext = ".zip")
  extract_dir <- tempfile(pattern = "pums_")

  dir.create(extract_dir)

  on.exit(
    unlink(
      c(zip_path, extract_dir),
      recursive = TRUE
    ),
    add = TRUE
  )

  # Download state person-level PUMS file
  download.file(
    url = zip_url,
    destfile = zip_path,
    mode = "wb",
    quiet = TRUE
  )

  # Extract ZIP file
  unzip(
    zipfile = zip_path,
    exdir = extract_dir
  )

  # Find the extracted CSV
  person_csv <- list.files(
    path = extract_dir,
    pattern = "\\.csv$",
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(person_csv) != 1) {
    stop(
      "Expected one CSV in ",
      zip_name,
      " but found ",
      length(person_csv)
    )
  }

  # Read needed variables and convert their storage types
  pums <- readr::read_csv(
    file = person_csv,
    col_select = tidyselect::all_of(
      c(
        "ST",
        "AGEP",
        "RAC1P",
        "HISP",
        "PWGTP"
      )
    ),
    show_col_types = FALSE,
    progress = FALSE
  ) %>%
    mutate(
      ST = as.character(ST),
      AGEP = as.integer(AGEP),
      RAC1P = as.integer(RAC1P),
      HISP = as.integer(HISP),
      PWGTP = as.numeric(PWGTP)
    )

  # State FIPS code
  geoid_value <- stringr::str_pad(
    dplyr::first(pums$ST),
    width = 2,
    side = "left",
    pad = "0"
  )

  # Calculate weighted estimates
  pums %>%
    filter(
      between(AGEP, 15, 24)
    ) %>%
    summarise(
      total_15_24 = sum(
        PWGTP,
        na.rm = TRUE
      ),

      white_nh_15_24 = sum(
        PWGTP[
          RAC1P == 1 &
          HISP == 1
        ],
        na.rm = TRUE
      )
    ) %>%
    mutate(
      year = 2020L,
      GEOID = geoid_value,
      state_abbr = state_abbr,

      white_nh_15_24_prop =
        white_nh_15_24 / total_15_24,

      white_nh_15_24_pct =
        100 * white_nh_15_24_prop
    ) %>%
    select(
      year,
      GEOID,
      state_abbr,
      white_nh_15_24,
      total_15_24,
      white_nh_15_24_prop,
      white_nh_15_24_pct
    )
}

# ------------------------------------------------------------
# Test Alabama
# ------------------------------------------------------------

test_alabama <-
  get_state_white_nh_15_24_2020("AL")

test_alabama


#all states
state_white_nh_15_24_acs1_2020 <-
  purrr::map_dfr(
    state_abbrs,
    get_state_white_nh_15_24_2020
  )

view(state_white_nh_15_24_acs1_2020)


#add state names

state_crosswalk <-
  tidycensus::fips_codes %>%
  transmute(
    GEOID = as.character(state_code),
    state = state_name
  ) %>%
  distinct() %>%
  mutate(
    GEOID = stringr::str_pad(
      GEOID,
      width = 2,
      side = "left",
      pad = "0"
    )
  )

state_white_nh_15_24_acs1_2020 <-
  state_white_nh_15_24_acs1_2020 %>%
  left_join(
    state_crosswalk,
    by = "GEOID"
  ) %>%
  select(
    year,
    GEOID,
    state,
    state_abbr,
    white_nh_15_24,
    total_15_24,
    white_nh_15_24_prop,
    white_nh_15_24_pct
  ) %>%
  arrange(GEOID)

view(state_white_nh_15_24_acs1_2020)


#all data 2019 - 2024#


#2019 AC1 estimates
state_white_nh_acs1_19

#2020 AC1 Experimental estimates
state_white_nh_15_24_acs1_2020

#2021:2024 AC1 estimates
state_white_nh_15_24_acs1_export


#merge all data frames

state_white_nh_15_24_ac1_19_24 <- bind_rows(
  state_white_nh_acs1_19,
  state_white_nh_15_24_acs1_2020,
  state_white_nh_15_24_acs1_export
) |> arrange(state, year)

View(state_white_nh_15_24_ac1_19_24)


#export dataset file for ACS1 estimates (2019 - 2024)
write_csv(
  state_white_nh_15_24_ac1_19_24,
  "state_white_nh_15_24_proportions_acs1_2019_2024.csv"
)


# Combine into one long-format dataset
state_white_nh_15_24_combined <- bind_rows(
  acs1_export,
  acs5_export
) %>%
  arrange(state, acs_type, year)

# View combined file
View(state_white_nh_15_24_combined)


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

Years <- c("2019","2020","2021","2022", "2023", "2024", "2025")

#create data set by institution for each year
state_flag_white <- data.frame(
  Institution = rep(Institution, each = length(Years)),
  State = rep(State, each = length(Years)),
  Years = rep(Years, times = length(Institution))
)

head(state_flag_white)

#create new excel file
write_xlsx(state_flag_white, "/Users/jjmena7/Desktop/Dissertation Research/diss_r_analysis/Data/state_flagship_white.xlsx")

##Merge IPEDS data to state flagships data

#Notes: Accessible Ipeds data goes from 2020 - 2024
#(Update including 2019 data below - Sept. 17. 2026)
##variable of interest: 
#"Full-time students, Undergraduate, Degree/certificate-seeking, First-time"

#first need to develop estimates
#EF2020A.White total/EF2020A.Grand total

#Packages
library(tidyverse)
library(readxl)
library(janitor)

###1. load data 

#Set working directory
setwd("/Users/jjmena7/Desktop/Dissertation Research/diss_r_analysis/Data/flasgship")

#Load state flagship data
state_flagships <- read_excel("state_flagship_white.xlsx")

#Load Ipeds Data

iped_2019 <- read_csv("2019.csv")
iped_2020 <- read_csv("2020.csv")
iped_2021 <- read_csv("2021.csv")
iped_2022 <- read_csv("2022.csv")
iped_2023 <- read_csv("2023.csv")
iped_2024 <- read_csv("2024.csv")

#function to clean each Ipeds file
read_ipeds_year <- function(file_path) {
  
  target_level <- "Full-time students, Undergraduate, Degree/certificate-seeking, First-time"
  
  df <- read_csv(file_path, show_col_types = FALSE) %>%
    clean_names()
  
  # Identify year from filename, e.g., "2020.csv" -> 2020
  file_year <- str_extract(basename(file_path), "\\d{4}") %>%
    as.integer()
  
  # Find relevant IPEDS columns
  level_col <- names(df)[str_detect(names(df), "level_of_student")]
  total_col <- names(df)[str_detect(names(df), "grand_total")]
  white_col <- names(df)[str_detect(names(df), "white_total")]
  
  df |>
    filter(.data[[level_col]] == target_level) %>%
    transmute(
      unitid = as.character(unitid),
      ipeds_institution_name = str_squish(institution_name),
      year = as.integer(year),
      first_time_total = as.numeric(.data[[total_col]]),
      first_time_white = as.numeric(.data[[white_col]]),
      flagship_white_prop = first_time_white / first_time_total
    )
}


ipeds_files <- c("2019.csv", "2020.csv", "2021.csv", "2022.csv", "2023.csv", "2024.csv")

ipeds_panel <- map_dfr(ipeds_files, read_ipeds_year)

#check data
glimpse(ipeds_panel)
ipeds_panel |> count(year)

#checking for missing data in 2020 and 2021
ipeds_panel |>
  distinct(unitid, ipeds_institution_name, year) |>
  count(unitid, ipeds_institution_name, name = "n_years") |>
  arrange(n_years) |> print(n = 248)

#missing 2 years in Pennsylvania State University-Main Campus 

#merge by unit ID 

flagship_crosswalk <- tribble(
  ~state, ~institution, ~unitid, ~ipeds_institution_name,
  
  "Alabama", "University of Alabama, Tuscaloosa", "100751", "The University of Alabama",
  "Alaska", "University of Alaska Fairbanks", "102614", "University of Alaska Fairbanks",
  "Arizona", "University of Arizona", "104179", "University of Arizona",
  "Arkansas", "University of Arkansas Fayetteville", "106397", "University of Arkansas",
  "California", "University of California, Berkeley", "110635", "University of California-Berkeley",
  "Colorado", "University of Colorado Boulder", "126614", "University of Colorado Boulder",
  "Connecticut", "University of Connecticut, Storrs", "129020", "University of Connecticut",
  "Delaware", "University of Delaware", "130943", "University of Delaware",
  "Florida", "University of Florida", "134130", "University of Florida",
  "Georgia", "University of Georgia", "139959", "University of Georgia",
  "Hawaii", "University of Hawaiʻi at Mānoa", "141574", "University of Hawaii at Manoa",
  "Idaho", "University of Idaho", "142285", "University of Idaho",
  "Illinois", "University of Illinois at Urbana-Champaign", "145637", "University of Illinois Urbana-Champaign",
  "Indiana", "Indiana University Bloomington", "151351", "Indiana University-Bloomington",
  "Iowa", "The University of Iowa", "153658", "University of Iowa",
  "Kansas", "University of Kansas, Lawrence", "155317", "University of Kansas",
  "Kentucky", "University of Kentucky, Lexington", "157085", "University of Kentucky",
  "Louisiana", "Louisiana State University, Baton Rouge", "159391", "Louisiana State University and Agricultural & Mechanical College",
  "Maine", "The University of Maine, Orono", "161253", "University of Maine",
  "Maryland", "University of Maryland, College Park", "163286", "University of Maryland-College Park",
  "Massachusetts", "University of Massachusetts Amherst", "166629", "University of Massachusetts-Amherst",
  "Michigan", "University of Michigan, Ann Arbor", "170976", "University of Michigan-Ann Arbor",
  "Minnesota", "University of Minnesota, Twin Cities", "174066", "University of Minnesota-Twin Cities",
  "Mississippi", "University of Mississippi", "176017", "University of Mississippi",
  "Missouri", "University of Missouri, Columbia", "178396", "University of Missouri-Columbia",
  "Montana", "University of Montana, Missoula", "180489", "The University of Montana",
  "Nebraska", "University of Nebraska–Lincoln", "181464", "University of Nebraska-Lincoln",
  "Nevada", "University of Nevada, Reno", "182290", "University of Nevada-Reno",
  "New Hampshire", "University of New Hampshire, Durham", "183044", "University of New Hampshire-Main Campus",
  "New Jersey", "Rutgers University–New Brunswick", "186380", "Rutgers University-New Brunswick",
  "New Mexico", "The University of New Mexico, Albuquerque", "187985", "University of New Mexico-Main Campus",
  "New York", "University at Buffalo (SUNY)", "196088", "University at Buffalo",
  "North Carolina", "The University of North Carolina at Chapel Hill", "199120", "University of North Carolina at Chapel Hill",
  "North Dakota", "University of North Dakota, Grand Forks", "200280", "University of North Dakota",
  "Ohio", "The Ohio State University, Columbus", "204796", "Ohio State University-Main Campus",
  "Oklahoma", "The University of Oklahoma, Norman Campus", "207500", "University of Oklahoma-Norman Campus",
  "Oregon", "University of Oregon, Eugene", "209551", "University of Oregon",
  "Pennsylvania", "Pennsylvania State University", "214777", "Pennsylvania State University-Main Campus",
  "Rhode Island", "The University of Rhode Island", "217484", "University of Rhode Island",
  "South Carolina", "University of South Carolina, Columbia", "218663", "University of South Carolina-Columbia",
  "South Dakota", "University of South Dakota, Vermillion", "219471", "University of South Dakota",
  "Tennessee", "The University of Tennessee, Knoxville", "221759", "The University of Tennessee-Knoxville",
  "Texas", "The University of Texas at Austin", "228778", "The University of Texas at Austin",
  "Utah", "The University of Utah", "230764", "University of Utah",
  "Vermont", "The University of Vermont", "231174", "University of Vermont",
  "Virginia", "University of Virginia, Charlottesville", "234076", "University of Virginia-Main Campus",
  "Washington", "University of Washington", "236948", "University of Washington-Seattle Campus",
  "West Virginia", "West Virginia University, Morgantown", "238032", "West Virginia University",
  "Wisconsin", "University of Wisconsin–Madison", "240444", "University of Wisconsin-Madison",
  "Wyoming", "University of Wyoming", "240727", "University of Wyoming"
)

#Merge IPEDS panel data (2019 -2024) with states

state_lookup <- flagship_crosswalk |>
  transmute(
    unitid = as.character(unitid),
    state
  ) %>%
  distinct(unitid, .keep_all = TRUE)

ipeds_panel <- ipeds_panel |>
  mutate(unitid = as.character(unitid)) |>
  left_join(state_lookup, by = "unitid")


#rearrange columns
ipeds_panel <- ipeds_panel |>
  select(
    unitid,
    year,
    state,
    ipeds_institution_name,
    first_time_total,
    first_time_white,
    flagship_white_prop
  )


#check for any missing data
ipeds_panel |>
  filter(year <= 2024, is.na(first_time_total)) |>
  distinct(state, ipeds_institution_name, unitid, year)

#Save as new csv file
#Save as CSV
#
write_csv(ipeds_panel, "state_flagships_ipeds_19_24.csv")

##### Create White Demographic representation & annual change estimates ####

#load data to be used
ac1 <- read_csv("/Users/jjmena7/Desktop/Dissertation Research/diss_r_analysis/Data/Racial Threat/acs1_state_white_nh_15_24_2019_2024.csv")
ac5 <- read_csv("/Users/jjmena7/Desktop/Dissertation Research/diss_r_analysis/Data/Racial Threat/acs5_state_white_nh_15_24_2019_2024.csv")

ipeds_panel <- read_csv("/Users/jjmena7/Desktop/Dissertation Research/diss_r_analysis/Data/flasgship/state_flagships_ipeds_19_24.csv")


#Data frames to be used:

#flagship white proportion estimates
ipeds_panel

#white non-Hispanic 15-24 AC1 & AC5 year estimates
ac1
ac5



# 1. Validate and standardize the three input datasets
ipeds_base <- ipeds_panel |>
  transmute(
    unitid = as.integer(unitid),
    year = as.integer(year),
    state,
    ipeds_institution_name,
    first_time_total,
    first_time_white,
    flagship_white_prop
  )

acs1_lookup <- ac1 |>
  transmute(
    state,
    year = as.integer(year),
    state_white_prop_acs1 = white_nh_15_24_prop
  )

acs5_lookup <- ac5 |>
  transmute(
    state,
    year = as.integer(year),
    acs5_period = acs_period,
    state_white_prop_acs5 = white_nh_15_24_prop
  )

#check that each acs data have one estimation per year
acs1_lookup |>
  count(state, year) |>
  filter(n > 1)

acs5_lookup |>
  count(state, year) |>
  filter(n > 1)

# 2. Create a final 2019–2024 IPEDS panel

ipeds_balanced <- ipeds_base |>
  group_by(unitid) |>
  complete(year = 2019:2024) |>
  fill(
    state,
    ipeds_institution_name,
    .direction = "downup"
  ) |>
  ungroup() |>
  arrange(unitid, year)



# 3. Join ACS estimates and calculate white demographic gaps

analysis_panel <- ipeds_balanced |>
  left_join(
    acs1_lookup,
    by = c("state", "year"),
    relationship = "many-to-one"
  ) |>
  left_join(
    acs5_lookup,
    by = c("state", "year"),
    relationship = "many-to-one"
  ) |>
  mutate(
    white_demo_rep_acs1 =
      flagship_white_prop - state_white_prop_acs1,

    white_demo_rep_acs5 =
      flagship_white_prop - state_white_prop_acs5
  ) |>
  relocate(
    state_white_prop_acs1,
    state_white_prop_acs5,
    white_demo_rep_acs1,
    white_demo_rep_acs5,
    .after = flagship_white_prop
  ) |>
  arrange(unitid, year)

#variable calcualtion
#white_demo_rep_acs5 = flagship_white_prop - state_white_prop_acs5
#white_demo_rep_acs1 = flagship_white_prop - state_white_prop_acs1


#check missing data
analysis_panel |>
  summarise(
    observations = n(),
    institutions = n_distinct(unitid),
    states = n_distinct(state),
    years = n_distinct(year),
    missing_acs1 = sum(is.na(state_white_prop_acs1)),
    missing_acs5 = sum(is.na(state_white_prop_acs5)),
    missing_ipeds = sum(is.na(flagship_white_prop)),
    missing_rep_acs1 = sum(is.na(white_demo_rep_acs1)),
    missing_rep_acs5 = sum(is.na(white_demo_rep_acs5))
  )


#======================================================
# 5. Calculate annual changes and lagged annual changes
#======================================================

analysis_panel <- analysis_panel |>
  arrange(unitid, year) |>
  group_by(unitid) |>
  mutate(
    # Annual change from year t-1 to year t
    white_demo_change_acs1 =
      white_demo_rep_acs1 - lag(white_demo_rep_acs1),

    white_demo_change_acs5 =
      white_demo_rep_acs5 - lag(white_demo_rep_acs5),

    # Previous year's annual change
    lagged_white_demo_change_acs1 =
      lag(white_demo_change_acs1),

    lagged_white_demo_change_acs5 =
      lag(white_demo_change_acs5)
  ) |>
  ungroup()


#load 2018 data
iped_2018 <- read_csv("/Users/jjmena7/Desktop/Dissertation Research/diss_r_analysis/Data/flasgship/2018.csv")

#acs5 2018 - 2024
state_white_nh_15_24_export

#acs1 2018 - 2024
state_white_nh_acs1_18


#============================
#1. prepare IPEDS 2018 data
#============================

ipeds_2018_clean <- iped_2018 |>
  transmute(
    unitid = as.integer(unitid),
    year = as.integer(year),
    ipeds_institution_name = `institution name`,
    first_time_total = `EF2018A_RV.Grand total`,
    first_time_white = `EF2018A_RV.White total`
  ) |>
  mutate(
    flagship_white_prop =
      first_time_white / first_time_total
  ) |>
  left_join(
    state_lookup |>
      mutate(unitid = as.integer(unitid)),
    by = "unitid",
    relationship = "many-to-one"
  ) |>
  select(
    unitid,
    year,
    state,
    ipeds_institution_name,
    first_time_total,
    first_time_white,
    flagship_white_prop
  )


#check
nrow(ipeds_2018_clean)
# Expected: 50
sum(is.na(ipeds_2018_clean$state))
# Expected: 0

#==============================================
#2. combine 2018 with exisitng ipeds panel data
#===============================================

ipeds_2019_2024 <- ipeds_panel |>
  transmute(
    unitid = as.integer(unitid),
    year = as.integer(year),
    state,
    ipeds_institution_name,
    first_time_total,
    first_time_white,
    flagship_white_prop
  ) |>
  filter(year %in% 2019:2024)

ipeds_base <- bind_rows(
  ipeds_2018_clean,
  ipeds_2019_2024
) |>
  arrange(unitid, year)

#check for duplicates
ipeds_base |>
  count(unitid, year) |>
  filter(n > 1)

#none should return

#=============================
#3. Create 2018 - 2024 panel
#=============================

ipeds_balanced <- ipeds_base |>
  group_by(unitid) |>
  complete(year = 2018:2024) |>
  fill(
    state,
    ipeds_institution_name,
    .direction = "downup"
  ) |>
  ungroup() |>
  arrange(unitid, year)


#=================================
#prepare ACS data
#=================================

#add 2018 acs1 estimates
acs1_2018 <- state_white_nh_acs1_18 |>
  transmute(
    state,
    year = as.integer(year),
    white_nh_15_24_prop
  ) |>
  filter(year == 2018)

ac1_all <- bind_rows(
  ac1 |>
    transmute(
      state,
      year = as.integer(year),
      white_nh_15_24_prop
    ),
  acs1_2018
) |>
  distinct(state, year, .keep_all = TRUE) |>
  arrange(state, year)

ac1_all |>
  count(year)

acs1_lookup <- ac1_all |>
  transmute(
    state,
    year = as.integer(year),
    state_white_prop_acs1 = white_nh_15_24_prop
  ) |>
  distinct(state, year, .keep_all = TRUE) |>
  arrange(state, year)

acs1_lookup |>
  count(year)

#acs 5 merging 2018 estimates 
acs5_lookup <- state_white_nh_15_24_export |>
  transmute(
    state,
    year = as.integer(year),
    acs5_period = acs_period,
    state_white_prop_acs5 = white_nh_15_24_prop
  ) |>
  filter(year %in% 2018:2024) |>
  distinct(state, year, .keep_all = TRUE) |>
  arrange(state, year)

acs5_lookup |>
  count(year)

#rebuild pandel data
analysis_panel <- ipeds_balanced |>
  left_join(
    acs1_lookup,
    by = c("state", "year"),
    relationship = "many-to-one"
  ) |>
  left_join(
    acs5_lookup,
    by = c("state", "year"),
    relationship = "many-to-one"
  ) |>
  mutate(
    white_demo_rep_acs1 =
      flagship_white_prop - state_white_prop_acs1,

    white_demo_rep_acs5 =
      flagship_white_prop - state_white_prop_acs5
  ) |>
  arrange(unitid, year)

#check whether 2018 is included
analysis_panel |>
  filter(year == 2018) |>
  summarise(
    observations = n(),
    missing_acs1 = sum(is.na(state_white_prop_acs1)),
    missing_acs5 = sum(is.na(state_white_prop_acs5))
  )

# ============================================================
# 5. Calculate annual and lagged annual demographic change
# ============================================================

analysis_panel <- analysis_panel |>
  arrange(unitid, year) |>
  group_by(unitid) |>
  mutate(
    # Change in representation gap from t-1 to t
    white_demo_change_acs1 =
      white_demo_rep_acs1 - lag(white_demo_rep_acs1),

    white_demo_change_acs5 =
      white_demo_rep_acs5 - lag(white_demo_rep_acs5),

    # Previous year's annual change
    lagged_white_demo_change_acs1 =
      lag(white_demo_change_acs1),

    lagged_white_demo_change_acs5 =
      lag(white_demo_change_acs5)
  ) |>
  ungroup() |>
  relocate(
    white_demo_change_acs1,
    white_demo_change_acs5,
    lagged_white_demo_change_acs1,
    lagged_white_demo_change_acs5,
    .after = white_demo_rep_acs5
  ) |>
  arrange(unitid, year)

#check on state
analysis_panel |>
  filter(state == "Alabama") |>
  select(
    state,
    year,
    white_demo_rep_acs1,
    white_demo_change_acs1,
    lagged_white_demo_change_acs1,
    white_demo_rep_acs5,
    white_demo_change_acs5,
    lagged_white_demo_change_acs5
  )

#check for 2020
analysis_panel |>
  filter(year == 2020) |>
  select(
    state,
    lagged_white_demo_change_acs1,
    lagged_white_demo_change_acs5
  )

#extract as csv file 
write_csv(analysis_panel, "state_racialthreat_indicators.csv")






#extract full panel data 





###################################################
# Higher Education Governance & Funding Variables 


#1.) First convert yes/no output from governance structure dataset to 1/0

#load packages 
library(readxl)
library(dplyr)
library(writexl)

#Set working directory 
setwd("/Users/jjmena7/Desktop/Dissertation Research/diss_r_analysis/Data/Higher Education")

#load higher ed governance structure dataset
gov_str <- read_excel("gov_structure.xlsx")


#inspect data 
names(gov_str)
glimpse(gov_str)

table(gov_str$ecs_single_statewide, useNA = "ifany")

#create (1/0) coding scheme 
#1 = 1 = state has a single statewide coordinating/governing board, 0 otherwise

gov_str.2 <- gov_str |> mutate(
  ecs_binary = case_when(
    ecs_single_statewide == "yes" ~1L,
    ecs_single_statewide == "no" ~ 0L,
    TRUE ~ NA_integer_
  )
)

#check the recoding with old data vs new 
table(
  gov_str.2$ecs_single_statewide,
  gov_str.2$ecs_binary,
  useNA = "ifany"
)

#check for any failed coding 
gov_str.2 |>
  filter(is.na(ecs_binary)) |>
  select(
    state,
    year,
    ecs_single_statewide
  )

#should return as onlyt tibble: 0 × 3

#select only few variables 

gov_str_bin <- gov_str.2 |> 
  select(
    state_id,
    state,
    state_abbr,
    year,
    ecs_single_statewide,
    ecs_binary
  )


#extract new coded dataset

write_xlsx(gov_str_bin, "/Users/jjmena7/Desktop/Dissertation Research/diss_r_analysis/Data/Higher Education/gov_str_bin.xlsx")



#######Higher Education Funding#####

#load packages 
library(tidyverse)
library(janitor)


#Set new Working directory
setwd("/Users/jjmena7/Desktop/Dissertation Research/diss_r_analysis/Data/Higher Education/state appropriations/raw_data")

#load and inspect 2019 appropriations data 
ipeds_2019 <- read_csv("ipeds_2019.csv",
                 show_col_types = FALSE) |> clean_names()

#check data 
names(ipeds_2019)

#variables names
#[1] "unitid"  "institution_name"                 
#[3] "year"    "f1819_f1a_rv_state_appropriations"

glimpse(ipeds_2019)

#check for each row having its own "unitid" should return back as zero
ipeds_2019 |>
  count(unitid) |>
  filter(n > 1)

#rename state appropriations variable 

ipeds_2019 <- ipeds_2019 |>
  rename(
    state_appropriations = f1819_f1a_rv_state_appropriations
  )


#repeat process for 2020 - 2024

#2020 IPEDS data 
ipeds_2020 <- read_csv("ipeds_2020.csv",
                 show_col_types = FALSE) |> clean_names()


names(ipeds_2020)
glimpse(ipeds_2020)

ipeds_2020 <- ipeds_2020 |>
  rename(
    state_appropriations = f1920_f1a_rv_state_appropriations
  )

#2021 IPEDS data 
ipeds_2021 <- read_csv("ipeds_2021.csv",
                 show_col_types = FALSE) |> clean_names()
names(ipeds_2021)
glimpse(ipeds_2021)

ipeds_2021 <- ipeds_2021 |>
  rename(
    state_appropriations = f2021_f1a_rv_state_appropriations
  )

#2022 IPEDS data
ipeds_2022 <- read_csv("ipeds_2022.csv",
                 show_col_types = FALSE) |> clean_names()
names(ipeds_2022)
glimpse(ipeds_2022)

ipeds_2022 <- ipeds_2022 |>
  rename(
    state_appropriations = f2122_f1a_rv_state_appropriations
  )

#2023 IPEDS data
ipeds_2023 <- read_csv("ipeds_2023.csv",
                 show_col_types = FALSE) |> clean_names()
names(ipeds_2023)
glimpse(ipeds_2023)

ipeds_2023 <- ipeds_2023 |>
  rename(
    state_appropriations = f2223_f1a_rv_state_appropriations
  )

#2024 IPEDS data
ipeds_2024 <- read_csv("ipeds_2024.csv",
                 show_col_types = FALSE) |> clean_names()
names(ipeds_2024)
glimpse(ipeds_2024)

ipeds_2024 <- ipeds_2024 |>
  rename(
    state_appropriations = f2324_f1a_state_appropriations
  )

#once all files have the same variables/names I can stack them 

ipeds_total <- bind_rows(
  ipeds_2019,
  ipeds_2020,
  ipeds_2021,
  ipeds_2022,
  ipeds_2023,
  ipeds_2024
)

glimpse(ipeds_total)
#provides a table count of observations by year
table(ipeds_total$year)

#load state charcteristics file 
state_characteristics <- read_csv(
  "state_characteristics.csv",
  show_col_types = FALSE
) |>
  clean_names()

names(state_characteristics)

#unitID to state crosswalk

ipeds_crosswalk <- state_characteristics |>
  select(
    unitid,
    state = hd2025_state_abbreviation_4
  ) |>
  distinct()


#merge state characteristics with state appropriations data 

ipeds_merged <- ipeds_total |>
  left_join(
    ipeds_crosswalk,
    by = "unitid"
  )

#verify that the merge worked 
ipeds_merged  |>
  summarise(
    observations = n(),
    missing_state = sum(is.na(state))
  )

ipeds_merged  |>
  group_by(year) |>
  summarise(
    n_institutions = n(),
    n_missing_appropriations =
      sum(is.na(state_appropriations)),
    pct_missing =
      mean(is.na(state_appropriations)) * 100
  )

#Identify variables that are missing 

ipeds_merged |>
  filter(is.na(state_appropriations)) |>
  select(
    unitid,
    institution_name,
    state,
    year
  ) |>
  arrange(state, institution_name, year)

#Checking if same instituions missing data every year
ipeds_merged |>
  filter(is.na(state_appropriations)) |>
  count(
    unitid,
    institution_name,
    state,
    name = "n_years_missing"
  ) |>
  arrange(desc(n_years_missing))

#checks for missing data 

# 1. Which institutions are missing?
ipeds_merged |>
  filter(is.na(state_appropriations)) |>
  select(unitid, institution_name, state, year) |>
  arrange(state, institution_name, year)

# 2. Are the same institutions missing repeatedly?
ipeds_merged |>
  filter(is.na(state_appropriations)) |>
  count(unitid, institution_name, state,
        name = "n_years_missing") |>
  arrange(desc(n_years_missing))

# 3. Which state-years contain missing institutions?
ipeds_merged |>
  filter(is.na(state_appropriations)) |>
  count(state, year)


#check states included
n_distinct(ipeds_merged$state)

#all 50 states from 2019 - 2024


#calculate total appropriations 
state_year_appropriations <- ipeds_merged |>
  group_by(
    state,
    year
  ) |>
  summarise(
    total_state_appropriations =
      sum(
        state_appropriations,
        na.rm = TRUE
      ),
    n_institutions = n(),
    n_missing_appropriations =
      sum(is.na(state_appropriations)),
    .groups = "drop"
  )



#adds state ID 

state_year_appropriations <- state_year_appropriations |>
  arrange(state, year) |>
  mutate(
    state_id = dense_rank(state) #assigns the same numeric ID to every observation with the same state name
  )

names(state_year_appropriations)

#slect order of final dataset
state_year_appropriations <- state_year_appropriations |> 
  select(
    state_id,
    state,
    year,
    total_state_appropriations,
    n_institutions,
    n_missing_appropriations
  )


#Extract to excell

write_xlsx(state_year_appropriations, "/Users/jjmena7/Desktop/Dissertation Research/diss_r_analysis/Data/Higher Education/state_year_appropriations.xlsx")


###################################################
# Diffusion Influences 

#Set working Directory
setwd("/Users/jjmena7/Desktop/Dissertation Research/diss_r_analysis/Data/Diffusion")

#Building neigboring States Data set
#Note: will use U.S. state polygons

#install needed packages#
#install.packages("sf")
#install.packages("tigris")

#Load packages 
library(sf) ## handles spatial/geographic data and lets us identify which states touch
library(tigris) # downloads Census geographic boundaries for U.S. states
library(dplyr)
library(tidyr)

#Get U.S. State Boundaries 
states_sf <- states(
  cb = TRUE,      # use simplified cartographic boundary files
  year = 2024     # use a recent Census boundary vintage
)

#inspect data
glimpse(states_sf)

#filter for only 50 states
states_50 <- states_sf |> 
  filter(
    STUSPS %in% state.abb
  ) |> 
  select(
    state_abbr = STUSPS,
    state_name = NAME,
    geometry
  )

#confirm 50 states 
nrow(states_50)

#Determine which states share borders
#identify Contiguous neigbors 
# st_touches() returns, for every state,
# the row numbers of states whose boundaries touch it.
border_list <- st_touches(states_50)

#Preview
border_list[1:5]

#Build a state-neighbor table 
state_neighbors <- tibble(
  focal_row = seq_len(nrow(states_50)),
  neighbor_row = border_list
) |>
  
  # Expand the list of neighbors into one row per neighbor
  unnest_longer(neighbor_row) |>
  
  # Attach focal-state abbreviation and name
  mutate(
    state_abbr = states_50$state_abbr[focal_row],
    state = states_50$state_name[focal_row],
    
    # Attach neighboring-state abbreviation and name
    neighbor_abbr = states_50$state_abbr[neighbor_row],
    neighbor = states_50$state_name[neighbor_row]
  ) |>
  
  # Keep only useful variables
  select(
    state_abbr,
    state,
    neighbor_abbr,
    neighbor
  ) |>
  
  # Sort for easy inspection
  arrange(state_abbr, neighbor_abbr)

state_neighbors
  
#count neigbors per state 

neighbor_counts <- state_neighbors |>
  count(
    state_abbr,
    state,
    name = "n_neighbors"
  ) |>
  arrange(state_abbr)

neighbor_counts

#validate for accuracy 
# Texas neighbors
state_neighbors |>
  filter(state_abbr == "TX")

state_neighbors |>
  filter(state_abbr == "AL")

state_neighbors |>
  filter(state_abbr == "CA")

#check for reciprocity 
#ofr example alabama and georgia should share eachother as neigbors 

reciprocity_check <- state_neighbors |>
  anti_join(
    state_neighbors |>
      transmute(
        state_abbr = neighbor_abbr,
        neighbor_abbr = state_abbr
      ),
    by = c("state_abbr", "neighbor_abbr")
  )

reciprocity_check

#save and extract state-neigbors dataset 

write.csv(
  state_neighbors,
  "state_neighbors.csv",
  row.names = FALSE
)

#Developing the regional diffusion variable 
#Following Johnson & Zhang (2020)

#packages needed 
#library(dplyr)
#library(tidyr)


# set working Directory 
setwd("/Users/jjmena7/Desktop/Dissertation Research/diss_r_analysis")

#load data DEI event dataset
dei_df <- read_excel("anti_dei_panel_data.xlsx", sheet = 3) |>
  clean_names() |>
  select(
    state_id,
    state,
    state_abbr,
    year,
    intro_any,
    intro_count,
    adopt_any
  )

#Inspect data
glimpse(dei_df)
glimpse(state_neighbors)

#observations x variables
dim(dei_df)
dim(state_neighbors)

#check for duplicates should be 0
state_neighbors |>
  count(state_abbr, neighbor_abbr) |>
  filter(n > 1)

#check event coding is 0/1
table(dei_df$intro_any, useNA = "ifany")
table(dei_df$adopt_any, useNA = "ifany")

#check for missing data 
dei_df |>
  summarise(
    missing_intro = sum(is.na(intro_any)),
    missing_adopt = sum(is.na(adopt_any))
  )

#CREATE PRIOR POLICY HISTORY 
#Before the focal year, had this state ever introduced?
#Before the focal year, had this state ever adopted?

dei_diffusion_reg <- dei_df |>
  arrange(state_id, year) |> #put observations in chronological order within states
  group_by(state_id) |> #calculate separately for each state
  mutate(
    ever_intro_current = cummax(intro_any), #Becomes 1 in the first year a state introduces and remains 1 afterward
    prior_intro = lag( #Lag the cumulative indicator by one year, tells whether the state had introduced BEFORE the current year
      ever_intro_current,
      n = 1,
      default = 0
    ),
    ever_adopt_current = cummax(adopt_any), #becomes 1 when the state first adopts and stays 1 afterward
    prior_adopt = lag( #whether the state had adopted BEFORE the current year
      ever_adopt_current,
      n = 1,
      default = 0
    )
  ) |> 
  ungroup()

#check new dataset created
dei_diffusion_reg |>
  select(
    state,
    year,
    intro_any,
    ever_intro_current,
    prior_intro,
    adopt_any,
    ever_adopt_current,
    prior_adopt
  ) |>
  arrange(state, year) |>
  print(n = 30)

#create neighbor policy status 
neighbor_status <- dei_diffusion_reg |>
  select(
    state_abbr,
    year,
    prior_intro,
    prior_adopt
  )

# Inspect
head(neighbor_status, 20)


#Create state + year data
focal_state_years <- dei_diffusion_reg |>
  select(
    state_abbr,
    year
  ) |>
  distinct()

head(focal_state_years)

#attach neighbor to each state-year
neighbor_year_data <- focal_state_years |>
  
  # Attach every contiguous neighbor belonging to the focal state
  inner_join(
    state_neighbors |>
      select(state_abbr, neighbor_abbr) |>
      distinct(),
    by = "state_abbr"
  )

#check Texas as an example 
neighbor_year_data |>
  filter(state_abbr == "TX") |>
  arrange(year, neighbor_abbr)

#attach each neighbor prior policy history
neighbor_year_data <- neighbor_year_data |>
  left_join(
    # Rename variables so it is obvious these describe
    # the NEIGHBOR rather than the focal state.
    neighbor_status |>
      rename(
        neighbor_abbr = state_abbr,
        neighbor_prior_intro = prior_intro,
        neighbor_prior_adopt = prior_adopt
      ),
    by = c(
      "neighbor_abbr",
      "year"
    )
  )

#inspect data 
neighbor_year_data |>
  filter(state_abbr == "TX") |>
  arrange(year, neighbor_abbr)

#CALCULATE THE INTRODUCTION DIFFUSION MEASURE 

#adjecent Introduction = (prior introduced bills / total # of contiguous states)

neighbor_diffusion <- neighbor_year_data |>
  group_by(state_abbr, year) |>
  summarise(
    # Number of contiguous neighboring states
    n_neighbors = n_distinct(neighbor_abbr),
    # Number of those neighboring states that had
    # introduced before the focal year
    n_neighbors_prior_intro = sum(
      neighbor_prior_intro == 1,
      na.rm = TRUE
    ),
    # PROPORTION of neighbors that had introduced
    # before the focal year
    adjacent_intro_prop =
      n_neighbors_prior_intro / n_neighbors,
    .groups = "drop"
  )

#check measure 
neighbor_diffusion |>
  arrange(state_abbr, year)

##CALCULATE THE ADOPTION/ENACTMENT DIFFUSION MEASURE 
neighbor_diffusion <- neighbor_year_data |>
  group_by(state_abbr, year) |>
  summarise(
    # NUMBER OF NEIGHBORS
    n_neighbors = n_distinct(neighbor_abbr),
    # INTRODUCTION DIFFUSION
    # Number of neighbors that previously introduced
    n_neighbors_prior_intro = sum(
      neighbor_prior_intro == 1,
      na.rm = TRUE
    ),
    # Proportion of neighbors that previously introduced
    adjacent_intro_prop =
      n_neighbors_prior_intro / n_neighbors,
    # ADOPTION / ENACTMENT DIFFUSION
    # Number of neighboring states that previously adopted
    # This variable useful for checking data,
    # even though it is not final adoption measure.
    n_neighbors_prior_adopt = sum(
      neighbor_prior_adopt == 1,
      na.rm = TRUE
    ),
    # Final adoption diffusion variable:
    #
    # 1 = at least one neighbor previously adopted
    # 0 = no neighbor previously adopted
    adjacent_adopt_any = as.integer(
      any(neighbor_prior_adopt == 1)
    ),
    .groups = "drop"
  )

#examine regional diffusion dataset
neighbor_diffusion |>
  arrange(state_abbr, year) |>
  print(n = 50)

#verify manually for one state-year 
#texas
neighbor_year_data |>
  filter(
    state_abbr == "TX",
    year == 2024
  ) |>
  select(
    state_abbr,
    year,
    neighbor_abbr,
    neighbor_prior_intro,
    neighbor_prior_adopt
  )

#Alabama
neighbor_year_data |>
  filter(
    state_abbr == "AL",
    year == 2024
  ) |>
  select(
    state_abbr,
    year,
    neighbor_abbr,
    neighbor_prior_intro,
    neighbor_prior_adopt
  )


#compare should add up correctly to .75
#Texas
neighbor_diffusion |>
  filter(
    state_abbr == "TX",
    year == 2024
  )

#Alabama
neighbor_diffusion |>
  filter(
    state_abbr == "AL",
    year == 2024
  )

#Merge diffusion variables into DEI panel
dei_df_diffusion <- dei_diffusion_reg |>
  left_join(
    neighbor_diffusion,
    by = c(
      "state_abbr",
      "year"
    )
  )

#check data
dei_df_diffusion |>
  select(
    state,
    state_abbr,
    year,
    intro_any,
    adopt_any,
    adjacent_intro_prop,
    adjacent_adopt_any
  ) |>
  arrange(state, year) |>
  print(n = 50)

#check Alaska & Hawaii should be NA
dei_df_diffusion |>
  filter(state_abbr %in% c("AK", "HI")) |>
  select(
    state,
    year,
    adjacent_intro_prop,
    adjacent_adopt_any
  )

#extract final regional diffusion variables 
write.csv(
  dei_df_diffusion,
  "dei_regional_diffusion.csv",
  row.names = FALSE
)

##PARTISAN DIFFUSION

#packages in use 
library(dplyr)

#load data DEI event dataset
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
    rep_leg_prop,
    rep_leg_prop_lag
  )

# Inspect the raw and lagged Republican legislative proportions
dei_df |>
  select(
    state_id,
    state,
    year,
    rep_leg_prop,
    rep_leg_prop_lag
  ) |>
  arrange(state_id, year) |>
  print(n = 30)

#create prior history variables for introduction and adoption
dei_partisan_base <- dei_df |>
  # Put state-year observations in chronological order
  arrange(state_id, year) |>
  # Perform calculations separately for each state
  group_by(state_id) |>
  mutate(
    # INTRODUCTION
    # Becomes 1 once the state has introduced at least one bill
    ever_intro_current = cummax(intro_any),
    # Indicates whether the state had already introduced
    # BEFORE the focal year.
    prior_intro = lag(
      ever_intro_current,
      n = 1,
      default = 0
    ),
    # ADOPTION
    # Becomes 1 once the state has adopted policy
    ever_adopt_current = cummax(adopt_any),
    # Indicates whether the state had already adopted
    # BEFORE the focal year.
    prior_adopt = lag(
      ever_adopt_current,
      n = 1,
      default = 0
    )
  ) |>
  ungroup()


#check for missing data in any of the variables of interest 
dei_partisan_base |>
  summarise(
    n_rows = n(),
    missing_rep_raw = sum(is.na(rep_leg_prop)),
    missing_rep_lag = sum(is.na(rep_leg_prop_lag)),
    missing_intro = sum(is.na(intro_any)),
    missing_adopt = sum(is.na(adopt_any)),
    missing_prior_intro = sum(is.na(prior_intro)),
    missing_prior_adopt = sum(is.na(prior_adopt))
  )

#create focal-state and peer states data set

#Focal-state 
# Each row represents the state whose partisan diffusion
# exposure to be calcualted

focal_states <- dei_partisan_base |>
  select(
    year,
    focal_state_id = state_id,
    focal_state = state,
    focal_abbr = state_abbr,
    focal_rep_lag = rep_leg_prop_lag
  )


# Peer-state version
# Each row represents a potential comparison state
peer_states <- dei_partisan_base |>
  select(
    year,
    peer_state_id = state_id,
    peer_state = state,
    peer_abbr = state_abbr,
    peer_rep_lag = rep_leg_prop_lag,
    peer_prior_intro = prior_intro,
    peer_prior_adopt = prior_adopt
  )

#next, create state pair combinations within same year
partisan_pairs <- focal_states |>
  # Join focal states to potential peer states
  # within the same year
  inner_join(
    peer_states,
    by = "year"
  ) |>
  # Remove self-comparisons
  # Example: Texas cannot be a peer of Texas
  filter(focal_state_id != peer_state_id)


#this creates a dataset with 49 possible pairs within each year (14,700 observations)



#CALCULATE PARTISAN SIMILIARITY/DISTANCE# 
partisan_pairs <- partisan_pairs |>
  mutate(
    # Absolute difference between focal state's
    # lagged Republican proportion and peer state's lagged Republican proportion.
    # Smaller = more politically similar
    partisan_distance = abs(
      focal_rep_lag - peer_rep_lag
    )
  )

#inspect
glimpse(partisan_pairs)

#RANK ALL SIMMILAR PEER STATES#
partisan_pairs_ranked <- partisan_pairs |>
  group_by(
    focal_state_id,
    focal_abbr,
    year
  ) |>
  # Sort from smallest partisan distance to largest
  arrange(
    partisan_distance,
    peer_abbr,
    .by_group = TRUE
  ) |>
  mutate(
    # Rank 1 = closest partisan peer
    # Rank 2 = second closest
    # etc
    partisan_rank = row_number()
  ) |>
  ungroup()

###Keep the closest third#
#from 49 states that would be approximately 16 states

#keep closest 16 state peers
partisan_peers <- partisan_pairs_ranked |>
  filter(partisan_rank <= 16)

#checking one state such as texas as an example 
partisan_peers |>
  filter(
    focal_abbr == "TX",
    year == 2024
  ) |>
  select(
    focal_abbr,
    year,
    focal_rep_lag,
    peer_abbr,
    peer_rep_lag,
    partisan_distance,
    partisan_rank,
    peer_prior_intro,
    peer_prior_adopt
  ) |>
  arrange(partisan_rank)


#check Florida

partisan_peers |>
  filter(
    focal_abbr == "FL",
    year == 2024
  ) |>
  select(
    focal_abbr,
    year,
    focal_rep_lag,
    peer_abbr,
    peer_rep_lag,
    partisan_distance,
    partisan_rank,
    peer_prior_intro,
    peer_prior_adopt
  ) |>
  arrange(partisan_rank)

#Missippi
partisan_peers |>
  filter(
    focal_abbr == "MS",
    year == 2024
  ) |>
  select(
    focal_abbr,
    year,
    focal_rep_lag,
    peer_abbr,
    peer_rep_lag,
    partisan_distance,
    partisan_rank,
    peer_prior_intro,
    peer_prior_adopt
  ) |>
  arrange(partisan_rank)

##CALCULATE FINAL PARTISAN DIFFUSION MEASURES
partisan_diffusion <- partisan_peers |>
  #Calculate diffusion separately for each focal state-year
  group_by(
    focal_state_id,
    focal_abbr,
    year
  ) |>
  summarise(
    ###NUMBER OF POLITICALLY SIMILAR PEERS
    #Should equal 16
    n_partisan_peers = n(),
    
    #PARTISAN INTRODUCTION DIFFUSION#
    #Number of partisan peers that had already introduced
    #an anti-DEI bill before the focal year
    n_partisan_prior_intro = sum(
      peer_prior_intro == 1,
      na.rm = TRUE
    ),
    #Proportion of partisan peers that had already introduced
    #Range:
    #0 = none of the 16 peers previously introduced
    #1 = all 16 peers previously introduced
    partisan_intro_prop =
      n_partisan_prior_intro / n_partisan_peers,
    
    #PARTISAN ADOPTION DIFFUSION
    # Number of partisan peers that had already adopted
    # anti-DEI policy before the focal year
    n_partisan_prior_adopt = sum(
      peer_prior_adopt == 1,
      na.rm = TRUE
    ),
    # Proportion of partisan peers that had already adopted
    partisan_adopt_prop =
      n_partisan_prior_adopt / n_partisan_peers,
    .groups = "drop"
  )


#Inspect the data
partisan_diffusion |>
  arrange(focal_abbr, year) |>
  print(n = 50)

#check for texas as an example
partisan_diffusion |>
  filter(
    focal_abbr == "TX"
  ) |>
  select(
    focal_abbr,
    year,
    n_partisan_peers,
    n_partisan_prior_intro,
    partisan_intro_prop,
    n_partisan_prior_adopt,
    partisan_adopt_prop
  ) 


partisan_diffusion |>
  count(n_partisan_peers)


#checks and compare to make sure caluculations are correct
partisan_peers |>
  filter(
    focal_abbr == "TX",
    year == 2024
  ) |>
  select(
    peer_abbr,
    partisan_rank,
    peer_prior_intro,
    peer_prior_adopt
  ) |>
  arrange(partisan_rank)


partisan_diffusion |>
  filter(
    focal_abbr == "TX",
    year == 2024
  )

#clean variable names for merging to full data set 
partisan_diffusion <- partisan_diffusion |>
  rename(
    state_id = focal_state_id,
    state_abbr = focal_abbr
  )

#extract final partisan diffusion variables 
write.csv(
  partisan_diffusion,
  "dei_partisan_diffusion.csv",
  row.names = FALSE
)







