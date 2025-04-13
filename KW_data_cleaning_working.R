# Setup ----
library(tidyverse)

# WICHE Data: High School Graduates by Year by State ----
wiche_data_allrows <- readxl::read_xlsx(
  path = "Data/Non-IPEDS Data/WICHE HS Graduates by State.xlsx",
  sheet = "Data"
)

hs_grads_by_year <- wiche_data_allrows %>%
  filter(
    Grade == "High school graduates", # Keep only rows ~ total high school graduates
    RaceEthnicity == "Total/any", # Keep only rows ~ total all race/ethnicity
    SchoolSector == "Grand Total (public+private)", # Keep only rows ~ total public and private high schools
    !(str_detect(string = Stabbr, pattern = "^_")), # Keep only rows NOT starting with an underscore (i.e., drop region aggregates)
    !(str_detect(string = Stabbr, pattern = "\\d")), # Keep only rows NOT containing digits (i.e., drop region aggregates)
    !(Stabbr %in% c("CM","CW")) # Keep only rows NOT CM and CW (i.e., drop region aggregates)
  ) %>%
  mutate(
    HS_Grad_Year = as.numeric(str_extract(string = ClassOf, pattern = "\\d+")) # Extract the graduation year for numeric filtering
  ) %>%
  filter(
    HS_Grad_Year >= 2017,
    HS_Grad_Year < 2026
  ) %>%
  select(
    # Keep only these variables
    Stabbr,
    HS_Grad_Year,
    Students
  ) %>%
  # Convert to wide format...
  pivot_wider(id_cols = Stabbr, names_from = HS_Grad_Year, values_from = Students, names_prefix = "HS_Grad_")

# ACS Data: Educational Attainment by State ----

acs_2018 <- read_csv(
  file = "Data/Non-IPEDS Data/ACS Educational Attainment by State/ACSST1Y2018.S1501-2025-04-09T015452.csv"
)
stabbr_csv <- read_csv(
  file = "Data/Non-IPEDS Data/states.csv"
)


educ_attain_2018 <- select(acs_2018, `Label (Grouping)`, ends_with("Percent!!Estimate"))[16,] %>%
  pivot_longer(cols = 2:53, names_to = "ACS_variable") %>%
  mutate(
    State = str_extract(string = ACS_variable, pattern = "^[^!]+(?=!)")
  ) %>%
  left_join(
    y = stabbr_csv,
    by = "State"
  ) %>%
  rename(ACS_Pct_Bachelors = value) %>%
  mutate(ACS_Pct_Bachelors = as.numeric(str_extract(ACS_Pct_Bachelors, pattern="\\d+\\.\\d+")))

# IPEDS: Geographic data ----

ipeds_geog_values <- read_csv(
  file = "Data/Data Geographic Variables/Data Geographic Variables.csv", col_select = 1:9
)
ipeds_geog_labels <- read_csv(
  file = "Data/Data Geographic Variables/Labels Geographic Variables.csv"
)

# The analysis variables for the geographic data are the following:
# Bureau of Economic Analysis (BEA) regions (HD2023)
# Degree of urbanization (Urban-centric locale) (HD2023)
# Keep state abbreviation `State abbreviation (HD2023)` for joining the other two tables

ipeds_geog_values$BEA_Region <- factor(
  x = ipeds_geog_values$`Bureau of Economic Analysis (BEA) regions (HD2023)`,
  levels = filter(ipeds_geog_labels, VariableName=="Bureau of Economic Analysis (BEA) regions (HD2023)")$Value %>% as.numeric(),
  labels = filter(ipeds_geog_labels, VariableName=="Bureau of Economic Analysis (BEA) regions (HD2023)")$ValueLabel
)

ipeds_geog_values$Locale <- factor(
  x = ipeds_geog_values$`Degree of urbanization (Urban-centric locale) (HD2023)`,
  levels = filter(ipeds_geog_labels, VariableName=="Degree of urbanization (Urban-centric locale) (HD2023)")$Value %>% as.numeric(),
  labels = filter(ipeds_geog_labels, VariableName=="Degree of urbanization (Urban-centric locale) (HD2023)")$ValueLabel
)

ipeds_geog_values$Locale_Rural <- case_when(
  ipeds_geog_values$`Degree of urbanization (Urban-centric locale) (HD2023)` %in% c(11,12,13,21,22,23) ~ "City/Suburb",
  ipeds_geog_values$`Degree of urbanization (Urban-centric locale) (HD2023)` %in% c(31,32,33,41,42,43) ~ "Town/Rural",
  TRUE ~ NA
)

# Joining geographic tables together ----

ipeds_geog_data <- ipeds_geog_values %>%
  select(
    UnitID,
    `Institution Name`,
    `State abbreviation (HD2023)`,
    BEA_Region,
    Locale,
    Locale_Rural
  ) %>%
  left_join(
    y = hs_grads_by_year,
    by = c("State abbreviation (HD2023)"="Stabbr")
  ) %>%
  left_join(
    y = educ_attain_2018 %>% select(Abbreviation, ACS_Pct_Bachelors),
    by = c("State abbreviation (HD2023)"="Abbreviation")
  )

# Calculate descriptive statistics ----
map(
  .x = ipeds_geog_data %>% select(BEA_Region, Locale, HS_Grad_2017, ACS_Pct_Bachelors),
  .f = function(x) {
    if(is.factor(x)) {
      x_tbl <- table(x, useNA = "ifany")
      
      tibble(
        Values = names(x_tbl),
        Val_N = x_tbl,
        Val_Pct = 100*round(x_tbl / sum(x_tbl), 3)
      ) %>%
        arrange(desc(Val_Pct))
    } else if(is.numeric(x)) {
      list(
        fivenum(x),
        c(mean(x), sd(x))
      )
    }
  }
)
