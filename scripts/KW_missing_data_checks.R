# Missing Data Checks

library(tidyverse)
full_dat3 <- read_csv("./Data/full_dat3.csv")


full_dat3_yr <- full_dat3 %>%
  # In the Carnegie universe
  filter(carnegie != "Not Carnegie") %>%
  # At least 1000 overall enrollment in first reporting year
  group_by(unit_id) %>%
  filter(year == min(year)) %>%
  ungroup() %>%
  filter(overall_enrollment >= 1000) %>%
  distinct(unit_id) %>%
  left_join(
    y = full_dat3,
    by = "unit_id"
  ) %>%
  # Has all 5 years of data
  group_by(unit_id) %>%
  summarise(N_year = length(unique(year))) %>%
  ungroup() %>%
  left_join(
    y = full_dat3,
    by = "unit_id"
  )

full_dat3_yr_msg <- full_dat3_yr %>%
  mutate(
    Year_X = year - min(year)
  ) %>%
  select(unit_id, N_year, Year_X, first_time_enrollment) %>%
  pivot_wider(id_cols = c("unit_id","N_year"), names_from="Year_X", values_from="first_time_enrollment") %>%
  mutate(
    MissingPattern = paste0(
      if_else(is.na(`0`),0,1),
      if_else(is.na(`1`),0,1),
      if_else(is.na(`2`),0,1),
      if_else(is.na(`3`),0,1),
      if_else(is.na(`4`),0,1)
    )
  ) 

full_dat3_yr_msg %>%
  count(N_year, MissingPattern) %>%
  arrange(desc(n))

unit_id_missing <- filter(full_dat3_yr_msg, N_year < 5)
writeClipboard(paste0(unit_id_missing$unit_id, collapse=","))


# Read in flags?
# orig_data <- read_csv("Data/Data Sector-Enrollment Variables/Sector-Enrollment-Data.csv")
flags <- read_csv("Data/Data Sector-Enrollment Variables/Imputation_Flags.csv")



