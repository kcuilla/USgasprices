library(eia)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(scales)
library(lubridate)

# set EIA key
key <- Sys.getenv("EIA_API_KEY")
eia_set_key(key)

# regular gas series IDs
regular_ids <- c(
  "PET.EMM_EPMR_PTE_NUS_DPG.W", # US
  "PET.EMM_EPMR_PTE_YBOS_DPG.W", # BOSTON
  "PET.EMM_EPMR_PTE_YORD_DPG.W", # CHICAGO
  "PET.EMM_EPMR_PTE_YCLE_DPG.W", # CLEVELAND
  "PET.EMM_EPMR_PTE_YDEN_DPG.W", # DENVER
  "PET.EMM_EPMR_PTE_Y44HO_DPG.W", # HOUSTON
  "PET.EMM_EPMR_PTE_Y05LA_DPG.W", # LOS ANGELES
  "PET.EMM_EPMR_PTE_YMIA_DPG.W", # MIAMI
  "PET.EMM_EPMR_PTE_Y35NY_DPG.W", # NYC
  "PET.EMM_EPMR_PTE_Y05SF_DPG.W", # SAN FRANCISCO
  "PET.EMM_EPMR_PTE_Y48SE_DPG.W" # SEATTLE
)

# midgrade gas series IDs
midgrade_ids <- c(
  "PET.EMM_EPMM_PTE_NUS_DPG.W", # US
  "PET.EMM_EPMM_PTE_YBOS_DPG.W", # BOSTON
  "PET.EMM_EPMM_PTE_YORD_DPG.W", # CHICAGO
  "PET.EMM_EPMM_PTE_YCLE_DPG.W", # CLEVELAND
  "PET.EMM_EPMM_PTE_YDEN_DPG.W", # DENVER
  "PET.EMM_EPMM_PTE_Y44HO_DPG.W", # HOUSTON
  "PET.EMM_EPMM_PTE_Y05LA_DPG.W", # LOS ANGELES
  "PET.EMM_EPMM_PTE_YMIA_DPG.W", # MIAMI
  "PET.EMM_EPMM_PTE_Y35NY_DPG.W", # NYC
  "PET.EMM_EPMM_PTE_Y05SF_DPG.W", # SAN FRANCISCO
  "PET.EMM_EPMM_PTE_Y48SE_DPG.W" # SEATTLE
)

# premium gas series IDs
premium_ids <- c(
  "PET.EMM_EPMP_PTE_NUS_DPG.W", # US
  "PET.EMM_EPMP_PTE_YBOS_DPG.W", # BOSTON
  "PET.EMM_EPMP_PTE_YORD_DPG.W", # CHICAGO
  "PET.EMM_EPMP_PTE_YCLE_DPG.W", # CLEVELAND
  "PET.EMM_EPMP_PTE_YDEN_DPG.W", # DENVER
  "PET.EMM_EPMP_PTE_Y44HO_DPG.W", # HOUSTON
  "PET.EMM_EPMP_PTE_Y05LA_DPG.W", # LOS ANGELES
  "PET.EMM_EPMP_PTE_YMIA_DPG.W", # MIAMI
  "PET.EMM_EPMP_PTE_Y35NY_DPG.W", # NYC
  "PET.EMM_EPMP_PTE_Y05SF_DPG.W", # SAN FRANCISCO
  "PET.EMM_EPMP_PTE_Y48SE_DPG.W" # SEATTLE
)

# function to pull gas prices since 2004
get_gas_prices <- function(id, start = "20040101") {

  data <- eia::eia_series(id, start = start)

  x <- data %>%
    tidyr::unnest(., cols = data) %>%
    dplyr::select(location = description, date, year, month, week, value, updated) %>%
    dplyr::mutate(type = stringr::str_extract(location, fixed(c("Regular","Midgrade","Premium")))) %>%
    dplyr::mutate(location = gsub(paste0(c("Regular","Midgrade","Premium"),collapse = "|"),"", location)) %>%
    dplyr::mutate(updated = lubridate::date(updated)) %>%
    dplyr::mutate(location = dplyr::case_when(
    location == "Boston, MA  All Formulations Retail Gasoline Prices" ~ "Boston, MA",
    location == "Chicago  All Formulations Retail Gasoline Prices" ~ "Chicago, IL",
    location == "Cleveland, OH  All Formulations Retail Gasoline Prices" ~ "Cleveland, OH",
    location == "Denver  All Formulations Retail Gasoline Prices" ~ "Denver, CO",
    location == "Houston  All Formulations Retail Gasoline Prices" ~ "Houston, TX",
    location == "Los Angeles  All Formulations Retail Gasoline Prices" ~ "Los Angeles, CA",
    location == "Miami, FL  All Formulations Retail Gasoline Prices" ~ "Miami, FL",
    location == "New York City  All Formulations Retail Gasoline Prices" ~ "New York, NY",
    location == "San Francisco  All Formulations Retail Gasoline Prices" ~ "San Francisco, CA",
    location == "Seattle, WA  All Formulations Retail Gasoline Prices" ~ "Seattle, WA",
    location == "U.S.  All Formulations Retail Gasoline Prices" ~ "U.S.",
    TRUE ~ "NA"
  )) %>%
  dplyr::group_by(location) %>%
  tidyr::fill(value) %>%
  dplyr::mutate(value = round(value, digits = 2)) %>%
  tidyr::fill(type, .direction = "downup") %>%
  dplyr::ungroup()

  return(x)
}

# combine regular, midgrade, and premium together
gas_data <- purrr::map_df(regular_ids, get_gas_prices) %>%
  dplyr::bind_rows(purrr::map_df(midgrade_ids, get_gas_prices)) %>%
  dplyr::bind_rows(purrr::map_df(premium_ids, get_gas_prices))

# get record prices
record_prices <- gas_data %>%
  dplyr::group_by(location, type) %>%
  dplyr::mutate(max_value = max(value)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(max_value == value) %>%
  dplyr::group_by(location, type) %>%
  dplyr::select(location, max_date = date, type, max_value) %>%
  dplyr::slice(which.max(max_date)) %>%
  dplyr::ungroup()

# get most recent gas price for each city
recent_prices <- gas_data %>%
  dplyr::group_by(location, type) %>%
  dplyr::arrange(desc(date)) %>%
  dplyr::filter(date >= updated - 734) %>%
  dplyr::mutate(
    dollar_wow = value - dplyr::lead(value),
    dollar_mom = value - dplyr::lead(value, n = 4),
    dollar_yoy = value - dplyr::lead(value, n = 52)
  ) %>%
  dplyr::mutate(
    percent_wow = value/dplyr::lead(value) - 1,
    percent_mom = value/dplyr::lead(value, n = 4) - 1,
    percent_yoy = value/dplyr::lead(value, n = 52) - 1
  ) %>%
  dplyr::ungroup()

# create summary table
summary_table <- recent_prices %>%
  dplyr::inner_join(record_prices, by = c("location","type")) %>%
  dplyr::filter(date == max(date)) %>%
  dplyr::mutate(max_value = scales::dollar(max_value)) %>%
  dplyr::select(location, type, date, updated, value, max_date, max_value, dplyr::ends_with(c("wow", "mom", "yoy")))

# historical data
historical_data <- gas_data %>%
  dplyr::select(date, location, type, value) %>%
  tidyr::pivot_wider(names_from = "location", values_from = "value")

# save datasets
usethis::use_data(historical_data, overwrite = TRUE)
usethis::use_data(summary_table, overwrite = TRUE)
