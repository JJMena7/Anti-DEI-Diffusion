#################################################################
"Anti-DEI Project Analysis Introduction and Adoption Descriptives" 
#################################################################
#Date: 4.17.26

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

# =========================
# STEP 1: LOAD PACKAGES
# =========================

library(tidyverse)
library(janitor)
library(scales)
library(survival)
library(survminer)
library(readxl) #reads excel files

#install.packages("janitor")
#install.packages("gt")

# Optional for nicer tables
library(gt)

# =========================
# STEP 2: IMPORT DATA
# =========================

# Read in your state-year panel data
# Replace the file name/path with your actual file location
dei_df <- read_excel("anti_dei_panel_data.xlsx", sheet = 3)|>
  clean_names()# standardizes column names into snake_case


# =========================
# STEP 3: REMOVE COMPLETELY EMPTY COLUMNS
# =========================

# Keep only columns that are NOT entirely missing
dei_df <- dei_df |>
  select(where(~ !all(is.na(.))))

# Remove the final two columns by position
dei_df <- dei_df[, -c(8, 9)]

# Check dimensions again
dim(dei_df)

# Inspect names again
names(dei_df)

# Preview the cleaned dataset
glimpse(dei_df)

#data checks

#check panel structure 
dei_df |>
  count(year)

# Count rows by state
# You want about 6 rows per state (2020-2025)
dei_df |>
  count(state_id, state) |>
  arrange(n) |>
  print(n = 300)

# Check for duplicate state-year rows
# This should return 0 rows
dei_df |>
  count(state_id, year) |>
  filter(n > 1)

# Check event coding consistency
# intro_any and adopt_any should only be 0/1
# intro_count should be nonnegative
dei_df |>
  summarise(
    bad_intro_any = sum(!intro_any %in% c(0, 1), na.rm = TRUE),
    bad_adopt_any = sum(!adopt_any %in% c(0, 1), na.rm = TRUE),
    negative_intro_count = sum(intro_count < 0, na.rm = TRUE)
  )

# =========================
# STEP 4: FIX VARIABLE TYPES
# =========================

dei_df <- dei_df |>
  mutate(
    state_id = as.integer(state_id),
    state = as.character(state),
    state_abbr = as.character(state_abbr),
    year = as.integer(year),
    intro_any = as.integer(intro_any),
    intro_count = as.integer(intro_count),
    adopt_any = as.integer(adopt_any)
  )

# Verify structure
glimpse(dei_df)

# =========================
# STEP 4: descriptives & Visuals 
# =========================

# Annual descriptive summary
annual_summary <- dei_df |>
  group_by(year) |>
  summarise(
    n_states = n_distinct(state_id),
    states_intro_any = sum(intro_any == 1, na.rm = TRUE),
    pct_states_intro_any = mean(intro_any == 1, na.rm = TRUE),
    total_intro_count = sum(intro_count, na.rm = TRUE),
    mean_intro_count = mean(intro_count, na.rm = TRUE),
    states_adopt_any = sum(adopt_any == 1, na.rm = TRUE),
    pct_states_adopt_any = mean(adopt_any == 1, na.rm = TRUE),
    .groups = "drop"
  )

annual_summary
 #year n_states states_intro_any pct_states_intro_any total_intro_count mean_intro_count states_adopt_any
#  <int>    <int>            <int>                <dbl>             <int>            <dbl>            <int>
#1  2020       50                0                 0                    0             0                   0
#2  2021       50                0                 0                    0             0                   0
#3  2022       50                1                 0.02                 2             0.04                0
#4  2023       50               22                 0.44                50             1                   6
#5  2024       50               20                 0.4                 42             0.84                7
#6  2025       50               24                 0.48                52             1.04               11



#######################################################
#######################################################

#first plot on total anti-dei intoduction counts by year

ggplot(annual_summary, aes(x = year, y = total_intro_count)) +
  geom_col() +
  geom_text(aes(label = total_intro_count), vjust = -0.4) +
  scale_x_continuous(breaks = sort(unique(annual_summary$year))) +
  labs(
    title = "Total Anti-DEI Bill Introductions by Year",
    x = "Year",
    y = "Number of Bills Introduced"
  ) +
  theme_minimal(base_size = 12)

#STATES WITH ANY INTRODUCTION OR ADOPTION

# Convert the summary table from wide to long format so both lines can be plotted together
annual_long <- annual_summary |>
  select(year, states_intro_any, states_adopt_any) |>
  pivot_longer(
    cols = c(states_intro_any, states_adopt_any),
    names_to = "event_type",
    values_to = "n_states"
  ) |>
  mutate(
    event_type = recode(
      event_type,
      states_intro_any = "Any Introduction",
      states_adopt_any = "Any Adoption"
    )
  )

ggplot(annual_long, aes(x = year, y = n_states, group = event_type, linetype = event_type)) +
  geom_line(linewidth = 1) +   # trend line over time
  geom_point(size = 2) +       # point at each year
  scale_x_continuous(breaks = sort(unique(annual_long$year))) +
  labs(
    title = "Number of States with Anti-DEI Policy Activity by Year",
    x = "Year",
    y = "Number of States",
    linetype = NULL
  ) +
  theme_minimal(base_size = 12)


# =========================
# STEP 6: CUMULATIVE SPREAD OF INTRODUCTIONS AND ADOPTIONS
# =========================

# Create cumulative "ever introduced by this year" and "ever adopted by this year" indicators
cumulative_summary <- dei_df |>
  group_by(state_id, state, state_abbr) |>
  arrange(year, .by_group = TRUE) |>
  mutate(
    # Once a state has introduced, this stays 1 in later years
    ever_intro_by_year = cummax(intro_any),

    # Once a state has adopted, this stays 1 in later years
    ever_adopt_by_year = cummax(adopt_any)
  ) |>
  ungroup() |>
  group_by(year) |>
  summarise(
    cumulative_states_intro = sum(ever_intro_by_year, na.rm = TRUE),
    cumulative_states_adopt = sum(ever_adopt_by_year, na.rm = TRUE),
    .groups = "drop"
  )

cumulative_summary


# =========================
# STEP 6B: CUMULATIVE DIFFUSION PLOT
# =========================

# Convert cumulative summary to long format for plotting
cumulative_long <- cumulative_summary |>
  pivot_longer(
    cols = c(cumulative_states_intro, cumulative_states_adopt),
    names_to = "event_type",
    values_to = "n_states"
  ) |>
  mutate(
    event_type = recode(
      event_type,
      cumulative_states_intro = "Ever Introduced",
      cumulative_states_adopt = "Ever Adopted"
    )
  )

## Plot cumulative diffusion over time
ggplot(cumulative_long, aes(x = year, y = n_states, group = event_type, linetype = event_type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = sort(unique(cumulative_long$year))) +
  labs(
    title = "Cumulative Number of States Ever Introducing or Adopting Anti-DEI Policy",
    x = "Year",
    y = "Cumulative Number of States",
    linetype = NULL
  ) +
  theme_minimal(base_size = 12)


# =========================
# STEP 7: FIRST EVENT VARIABLES
# =========================

first_events <- dei_df |>
  group_by(state_id, state, state_abbr) |>
  summarise(
    # First year the state introduced at least one bill
    first_intro_year = ifelse(
      any(intro_any == 1, na.rm = TRUE),
      min(year[intro_any == 1], na.rm = TRUE),
      NA_integer_
    ),

    # First year the state adopted at least one policy
    first_adopt_year = ifelse(
      any(adopt_any == 1, na.rm = TRUE),
      min(year[adopt_any == 1], na.rm = TRUE),
      NA_integer_
    ),

    # 1 if the state ever introduced during 2020-2025
    ever_intro = as.integer(any(intro_any == 1, na.rm = TRUE)),

    # 1 if the state ever adopted during 2020-2025
    ever_adopt = as.integer(any(adopt_any == 1, na.rm = TRUE)),

    # Total number of bill introductions across the whole panel
    total_intro_count = sum(intro_count, na.rm = TRUE),

    # Number of years with at least one introduction
    years_with_intro = sum(intro_any == 1, na.rm = TRUE),

    # Number of years with at least one adoption
    years_with_adopt = sum(adopt_any == 1, na.rm = TRUE),

    .groups = "drop"
  ) |>
  mutate(
    # Convert first event year into years since 2020
    time_to_first_intro = if_else(
      !is.na(first_intro_year),
      first_intro_year - 2020,
      NA_integer_
    ),
    time_to_first_adopt = if_else(
      !is.na(first_adopt_year),
      first_adopt_year - 2020,
      NA_integer_
    )
  )

# Inspect result
glimpse(first_events)
head(first_events)


# =========================
# SUMMARIZE FIRST-EVENT TIMING
# =========================

# How many states first introduced in each year?
first_events |>
  count(first_intro_year, sort = FALSE)

# How many states first adopted in each year?
first_events |>
  count(first_adopt_year, sort = FALSE)

# Overall counts
first_events |>
  summarise(
    total_states = n(),
    ever_intro_states = sum(ever_intro, na.rm = TRUE),
    never_intro_states = sum(ever_intro == 0, na.rm = TRUE),
    ever_adopt_states = sum(ever_adopt, na.rm = TRUE),
    never_adopt_states = sum(ever_adopt == 0, na.rm = TRUE)
  )

#table

n_table <- dei_df |>
  group_by(year) |>
  summarise(
    n = n_distinct(state_id),
    .groups = "drop"
  )

# Step 2: count first introduction events by year
intro_table <- first_events |>
  filter(!is.na(first_intro_year)) |>
  count(first_intro_year, name = "intro_first_events") |>
  rename(year = first_intro_year)

# Step 3: count first adoption events by year
adopt_table <- first_events |>
  filter(!is.na(first_adopt_year)) |>
  count(first_adopt_year, name = "adopt_first_events") |>
  rename(year = first_adopt_year)

# Step 4: combine into one yearly table
event_table <- n_table |>
  left_join(intro_table, by = "year") |>
  left_join(adopt_table, by = "year") |>
  mutate(
    intro_first_events = replace_na(intro_first_events, 0L),
    adopt_first_events = replace_na(adopt_first_events, 0L)
  ) |>
  arrange(year)

# Step 5: add total row
total_row <- event_table |>
  summarise(
    year = "Total",
    n = sum(n, na.rm = TRUE),
    intro_first_events = sum(intro_first_events, na.rm = TRUE),
    adopt_first_events = sum(adopt_first_events, na.rm = TRUE)
  )

# Step 6: final table
event_table_final <- event_table |>
  mutate(year = as.character(year)) |>
  bind_rows(total_row)

print(event_table_final)

#table with polished labels
event_table_final |>
  gt() |>
  tab_header(
    title = "Annual First Events for Anti-DEI Bill Introduction and Adoption"
  ) |>
  cols_label(
    year = "Year",
    n = "Observations (n)",
    intro_first_events = "First Introductions",
    adopt_first_events = "First Adoptions"
  )

####################
#INTRODUCTIONS TABLE###### HERE!
####################
# Step 1: create dataset where states leave the risk set after first introduction
dei_intro_risk <- dei_df |>
  group_by(state_id) |>
  mutate(
    # first year of introduction for each state
    first_intro_year = if_else(
      any(intro_any == 1, na.rm = TRUE),
      min(year[intro_any == 1], na.rm = TRUE),
      NA_integer_
    )
  ) |>
  # keep rows up to and including the first introduction year
  filter(is.na(first_intro_year) | year <= first_intro_year) |>
  ungroup()
# Step 2: build annual table
intro_event_table <- dei_intro_risk |>
  group_by(year) |>
  summarise(
    # actual number of observations remaining in the intro risk set that year
    n = n(),

    # number of first introduction events in that year
    intro_first_events = sum(intro_any == 1, na.rm = TRUE),
    .groups = "drop"
  )
# Step 3: add total row
intro_total_row <- intro_event_table |>
  summarise(
    year = "Total",
    n = sum(n, na.rm = TRUE),
    intro_first_events = sum(intro_first_events, na.rm = TRUE)
  )
# Step 4: final table
intro_event_table_final <- intro_event_table |>
  mutate(year = as.character(year)) |>
  bind_rows(intro_total_row)

print(intro_event_table_final)

#################
#ADOPTION TABLE##
#################

# Step 1: create dataset where states leave the risk set after first adoption
dei_adopt_risk <- dei_df |>
  group_by(state_id) |>
  mutate(
    # first year of adoption for each state
    first_adopt_year = if_else(
      any(adopt_any == 1, na.rm = TRUE),
      min(year[adopt_any == 1], na.rm = TRUE),
      NA_integer_
    )
  ) |>
  # keep rows up to and including the first adoption year
  filter(is.na(first_adopt_year) | year <= first_adopt_year) |>
  ungroup()

# Step 2: build annual table
adopt_event_table <- dei_adopt_risk |>
  group_by(year) |>
  summarise(
    # actual number of observations remaining in the adopt risk set that year
    n = n(),

    # number of first adoption events in that year
    adopt_first_events = sum(adopt_any == 1, na.rm = TRUE),

    .groups = "drop"
  )

# Step 3: add total row
adopt_total_row <- adopt_event_table |>
  summarise(
    year = "Total",
    n = sum(n, na.rm = TRUE),
    adopt_first_events = sum(adopt_first_events, na.rm = TRUE)
  )

# Step 4: final table
adopt_event_table_final <- adopt_event_table |>
  mutate(year = as.character(year)) |>
  bind_rows(adopt_total_row)


print(adopt_event_table_final)

# =========================
# COMBINED TABLE: INTRODUCTION AND ADOPTION RISK SETS TABLE
# =========================

combined_event_table <- intro_event_table |>
  rename(
    n_intro_risk = n,
    intro_first_events = intro_first_events
  ) |>
  full_join(
    adopt_event_table |>
      rename(
        n_adopt_risk = n,
        adopt_first_events = adopt_first_events
      ),
    by = "year"
  ) |>
  arrange(year)

combined_total_row <- combined_event_table |>
  summarise(
    year = "Total",
    n_intro_risk = sum(n_intro_risk, na.rm = TRUE),
    intro_first_events = sum(intro_first_events, na.rm = TRUE),
    n_adopt_risk = sum(n_adopt_risk, na.rm = TRUE),
    adopt_first_events = sum(adopt_first_events, na.rm = TRUE)
  )

combined_event_table_final <- combined_event_table |>
  mutate(year = as.character(year)) |>
  bind_rows(combined_total_row)

print(combined_event_table_final)


# =========================
# STATE-LEVEL SUMMARY TABLE
# =========================

#Ranked state summary table
state_summary <- first_events |>
  arrange(first_intro_year, desc(total_intro_count), state)

state_summary

#more active states first
state_summary_intensity <- first_events |>
  arrange(desc(total_intro_count), first_intro_year, state)

state_summary_intensity

glimpse(state_summary_intensity)

# =========================
# STATE-YEAR HEATMAP OF INTRODUCTIONS
# =========================

# Order states by first introduction year, then by total introduction count
state_order_intro <- first_events |>
  arrange(first_intro_year, desc(total_intro_count), state) |>
  pull(state)

# Apply the state ordering to the panel
df_heat_intro <- dei_df |>
  mutate(state = factor(state, levels = rev(state_order_intro)))

# Plot heatmap
ggplot(df_heat_intro, aes(x = factor(year), y = state, fill = factor(intro_any))) +
  geom_tile(color = "white") +
  scale_fill_manual(
    values = c("0" = "grey90", "1" = "black"),
    name = "Introduction",
    labels = c("0" = "No", "1" = "Yes")
  ) +
  labs(
    title = "State-Year Heatmap of Anti-DEI Bill Introductions",
    x = "Year",
    y = "State"
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank())


#order by legislative intensity
state_order_intensity <- first_events |>
  arrange(desc(total_intro_count), first_intro_year, state) |>
  pull(state)

# Apply the state ordering to the panel
df_heat_intensity <- dei_df |>
  mutate(state = factor(state, levels = rev(state_order_intensity)))

# Plot heatmap
ggplot(df_heat_intensity, aes(x = factor(year), y = state, fill = factor(intro_any))) +
  geom_tile(color = "white") +
  scale_fill_manual(
    values = c("0" = "grey90", "1" = "black"),
    name = "Introduction",
    labels = c("0" = "No", "1" = "Yes")
  ) +
  labs(
    title = "State-Year Heatmap of Anti-DEI Bill Introductions",
    x = "Year",
    y = "State"
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank())

#It shows:
#who entered early
#who entered later
#whether activity is one-time or repeated
#whether the same states stay active across years

# =========================
#STATE-YEAR HEATMAP OF ADOPTIONS
# =========================

state_order_adopt <- first_events |>
  arrange(first_adopt_year, state) |>
  pull(state)

df_heat_adopt <- dei_df |>
  mutate(state = factor(state, levels = rev(unique(state_order_adopt))))

ggplot(df_heat_adopt, aes(x = factor(year), y = state, fill = factor(adopt_any))) +
  geom_tile(color = "white") +
  scale_fill_manual(
    values = c("0" = "grey90", "1" = "black"),
    name = "Adoption",
    labels = c("0" = "No", "1" = "Yes")
  ) +
  labs(
    title = "State-Year Heatmap of Anti-DEI Policy Adoption",
    x = "Year",
    y = "State"
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank())

# =========================
# CHECK FIRST-EVENT DATA
# =========================

# Look at structure of the state-level first-event dataset
glimpse(first_events)

# Check a few rows
head(first_events)

# Confirm number of rows (should be 50 states)
nrow(first_events)

# Confirm event counts
first_events |>
  summarise(
    ever_intro_states = sum(ever_intro, na.rm = TRUE),
    censored_intro_states = sum(ever_intro == 0, na.rm = TRUE),
    ever_adopt_states = sum(ever_adopt, na.rm = TRUE),
    censored_adopt_states = sum(ever_adopt == 0, na.rm = TRUE)
  )

# =========================
# STEP 2: Kaplan Meier READY VARIABLES
# =========================

km_data <- first_events |>
  mutate(
    # For introduction:
    # If the state introduced, use the first-event time
    # If not, censor at the end of the study window (2025 = 5 years after 2020)
    km_time_intro = if_else(ever_intro == 1, time_to_first_intro, 5L),
    km_event_intro = ever_intro,

    # For adoption:
    # Same logic
    km_time_adopt = if_else(ever_adopt == 1, time_to_first_adopt, 5L),
    km_event_adopt = ever_adopt
  )

# Inspect
km_data |>
  select(state, km_time_intro, km_event_intro, km_time_adopt, km_event_adopt) |>
  head(15)

#Here:

#baseline is 2020
#end of follow-up is 2025
#states without an event are censored at 5


# =========================
# KM CURVE FOR TIME TO FIRST INTRODUCTION
# =========================

# Create survival object for first introduction
surv_obj_intro <- Surv(
  time = km_data$km_time_intro,
  event = km_data$km_event_intro
)

# Fit Kaplan-Meier model
# ~ 1 means one overall curve for all states
km_fit_intro <- survfit(
  Surv(km_time_intro, km_event_intro) ~ 1,
  data = km_data
)

# Print KM summary in console
summary(km_fit_intro)

# Plot KM curve
ggsurvplot(km_fit_intro,
  conf.int = TRUE,       # show confidence interval around the survival curve
  risk.table = TRUE,     # show number of states still "at risk" below the figure
  censor = TRUE,         # mark censoring
  xlab = "Years Since 2020",
  ylab = "Probability of Not Yet Introducing a Bill",
  title = "Kaplan-Meier Curve: Time to First Anti-DEI Bill Introduction",
  break.time.by = 1
)

#“The Kaplan-Meier survival estimate for time to first
#anti-DEI bill introduction remains at 1.00 through 2021 
#and declines only minimally in 2022, indicating almost no 
#early diffusion. The sharpest decline occurs in 2023, 
#suggesting that first introductions were highly concentrated 
#in that year. By the end of the observation window, the survival 
#probability is approximately 0.40, indicating that 60% of states
#had experienced a first introduction and 40% remained right-censored.”


# =========================
# KAPLAN-MEIER: TIME TO FIRST ADOPTION
# =========================

# Fit Kaplan-Meier model for adoption
km_fit_adopt <- survfit(
  Surv(km_time_adopt, km_event_adopt) ~ 1,
  data = km_data
)

# Plot Kaplan-Meier curve
ggsurvplot(
  km_fit_adopt,
  data = km_data,
  conf.int = TRUE,
  risk.table = TRUE,
  censor = TRUE,
  xlab = "Years Since 2020",
  ylab = "Probability of Not Yet Adopting Policy",
  title = "Kaplan-Meier Curve: Time to First Anti-DEI Policy Adoption",
  break.time.by = 1
)


#Making both curves easier to interpret 
# =========================
# EXTRACT KM ESTIMATES
# =========================

# Introduction KM estimates
intro_km_table <- summary(km_fit_intro)

intro_results <- tibble(
  time = intro_km_table$time,
  n_risk = intro_km_table$n.risk,
  n_event = intro_km_table$n.event,
  survival_prob = intro_km_table$surv,
  lower_ci = intro_km_table$lower,
  upper_ci = intro_km_table$upper
)

intro_results

# Adoption KM estimates
adopt_km_table <- summary(km_fit_adopt)

adopt_results <- tibble(
  time = adopt_km_table$time,
  n_risk = adopt_km_table$n.risk,
  n_event = adopt_km_table$n.event,
  survival_prob = adopt_km_table$surv,
  lower_ci = adopt_km_table$lower,
  upper_ci = adopt_km_table$upper
)

adopt_results


#These tables let you write the results more precisely, for example:

#survival probability after 2023
#number of states still at risk before 2024
#number of first events occurring at each interval


#Your first-adoption counts were:

#2023: 6 states
#2024: 7 states
#2025: 6 states
#never adopted by 2025: 31 states

#Implies:

#19 total adoption events

#How to read the drops
#No drop through 2022
#No states adopted before 2023.
#First drop at 2023
#6 states had first adoption in 2023.
#Second drop at 2024
#7 more states first adopted in 2024.
#Third drop at 2025
#6 more states first adopted in 2025.
#Curve ends around 0.62
#About 62% of states had not adopted by 2025.
#Equivalently, about 38% had adopted by 2025, which matches 19/50.

#Interpretation:

#before 2023, all 50 states were still at risk of first adoption
#after 6 first adoptions in 2023, 44 remained at risk entering 2024
#after 7 first adoptions in 2024, 37 remained at risk entering 2025

#“The Kaplan-Meier survival function for time to first adoption remains at 1.00 
#through 2022, indicating no adoptions in the early panel years.”
#“The first decline occurs in 2023, followed by additional declines 
#in 2024 and 2025, suggesting a later and more selective diffusion process for 
#enactment than for introduction.”
#“By the end of the observation window, the survival probability remains near
#0.62, indicating that most states remained adoption-free and that enactment
#diffused less broadly than agenda-setting.”

# =========================
# SIDE-BY-SIDE KAPLAN-MEIER FIGURE
# =========================
#Create introduction plot
plot_intro <- ggsurvplot(
  km_fit_intro,
  data = km_data,
  conf.int = TRUE,          # show confidence interval
  risk.table = FALSE,       # turn off here so side-by-side figure stays clean
  censor = TRUE,            # show censoring marks
  legend = "none",
  xlab = "Years Since 2020",
  ylab = "Probability of Not Yet Experiencing Event",
  title = "First Introduction",
  break.time.by = 1
)

 #Create adoption plot
plot_adopt <- ggsurvplot(
  km_fit_adopt,
  data = km_data,
  conf.int = TRUE,
  risk.table = FALSE,
  censor = TRUE,
  legend = "none",
  xlab = "Years Since 2020",
  ylab = "Probability of Not Yet Experiencing Event",
  title = "First Adoption",
  break.time.by = 1
)

#side by side plot
ggarrange(
  plot_intro$plot,
  plot_adopt$plot,
  ncol = 2,
  nrow = 1
)












