# README
# Count of tests in sample by test type (and various other metrics)
# used for Q4/A4 table and also quoted in quarto report text.

library("here")
library("arrow")
library("dplyr")
library("tidyr")
library("readr")
library("forcats")
library("janitor")
library("stringr")
library("lubridate")

provider_sample <-
  open_dataset(here("data_raw", "provider_sample.parquet")) |>
  collect()

lkp_invst <- readRDS(here("reference", "lkp_invst.rds"))


# PULL EC TYPE CODES:
vec_invst_codes_ec <- lkp_invst |>
  mutate(InvestigationCode = as.character(InvestigationCode)) |>
  mutate(InvestigationKey = str_c("invst_", InvestigationKey)) |>
  select(InvestigationKey, InvestigationCode) |>
  tibble::deframe()

# NOTE: COUNTING INSTANCES HERE TO KEEP OPTIONS OPEN.
# BUT LATER ADJUSTED TO BINARY (TEST Y/N).
list_invst_counts <- imap(vec_invst_codes_ec, function(x, y) {
  provider_sample %>%
    transmute({{ y }} := str_count(Der_EC_Investigation_All, str_c("^", x, "| ", x, "|,", x)))
})

df_counts <- list_invst_counts |>
  reduce(bind_cols) |>
  bind_cols(provider_sample, y = _)

df_counts <- df_counts |>
  group_by(fyear) |>
  summarise(
    across(
      starts_with("invst_"),
      ~ sum(., na.rm = T)
    )
  ) |>
  ungroup() |>
  pivot_longer(cols = starts_with("invst"), names_to = "test", values_to = "n") |>
  arrange(-n) |>
  left_join(
    df_counts |>
      count(fyear, name = "n_att"),
    join_by(fyear)
  ) |>
  rename(n_tests = n)

df_counts |>
  arrange(fyear) |>
  pivot_wider(names_from = fyear, values_from = c(n_att, n_tests)) |>
  clean_names() |>
  mutate(test_growth = n_tests_2023_24 / n_tests_2019_20) |>
  mutate(test_rate_1920 = n_tests_2019_20 / n_att_2019_20) |>
  mutate(test_rate_2324 = n_tests_2023_24 / n_att_2023_24) |>
  mutate(rate_growth = test_rate_2324 / test_rate_1920) |>
  arrange(-rate_growth) |>
  select(-starts_with("test_r")) |>
  mutate(InvestigationKey = as.numeric(str_extract(test, "[:digit:]{2}"))) |>
  left_join(
    lkp_invst |>
      select(InvestigationKey, InvestigationDescription),
    join_by(InvestigationKey)
  ) |>
  relocate(InvestigationDescription, 1) |>
  saveRDS(here("data", "df_growth_counts.rds"))
