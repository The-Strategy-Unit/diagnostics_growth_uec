# README
# Analysis of the impact of multiple tests on time in ED.
# Here we ignore casemix and examine only the un-adjusted impact.

library("here")
library("arrow")
library("dplyr")
library("purrr")
library("tidyr")
library("scales")
library("tibble")
library("forcats")
library("janitor")
library("ggplot2")
library("stringr")
library("lubridate")


# 1. LOAD ECDS DATA -------------------------------------------------------

# source("create_study_dataset.R")

provider_sample <-
  open_dataset(here("data_raw", "provider_sample.parquet")) |>
  collect() |>
  mutate(dur_assess_depart = duration_ed - as.numeric(difftime(dttm_arr, dttm_assess, units = "mins"))) |>
  mutate(flag_odd_time = if_else(dttm_arr > dttm_assess, 1, 0)) |>
  mutate(flag_odd_time = if_else(dur_assess_depart < 0, 1, flag_odd_time)) |>
  # NOTE: WE MAY WANT TO BE EVEN MORE CONSERVATIVE HERE:
  mutate(flag_odd_time = if_else(dur_assess_depart >= 96 * 60, 1, flag_odd_time))
# 0.2% CASES WITH ODD TIME

gc()

# 2. CREATE INVESTIGATION LOOKUPS ------------------------------

lkp_invst <- readRDS(here("data_raw", "lkp_invst.RDS"))

lkp_test_ec_to_aea <- readRDS(here("data_raw", "lkp_test_ec_to_aea.RDS"))

lkp_invst <- lkp_invst |>
  select(InvestigationKey, InvestigationCode, InvestigationDescription) |>
  # # REMOVE NO INVESTIGATION FROM SEARCH LIST
  # (NOTE: ITS PRESENCE DOESN'T INDICATE NO INVESTIGATION):
  filter(InvestigationKey != 98) |>
  collect() |>
  left_join(
    lkp_test_ec_to_aea |>
      select(InvestigationKey, invst_group),
    join_by(InvestigationKey)
  )

# 3. CREATE DF FOR ANALYSIS -----------------------------------------------

df_multiple_tests_base <- provider_sample |>
  filter(is_march != 1) |>
  # TODO DO WE NEED THIS LATER?
  mutate(id = row_number()) |>
  select(-matches("disdest|^diag|Diagnosis|age_grp|lso|dur|^reg|^lacd|is_mar|^time_|^n_"), dur_assess_depart, disdest_grp) |>
  # 0.2% CASES WITH ODD TIME
  filter(flag_odd_time != 1)

df_multiple_tests_long <- df_multiple_tests_base |>
  select(id, Der_EC_Investigation_All) |>
  separate_longer_delim(Der_EC_Investigation_All, delim = ", ") |>
  rename(InvestigationCode = Der_EC_Investigation_All) |>
  mutate(InvestigationCode = as.integer(InvestigationCode)) |>
  left_join(
    lkp_invst,
    join_by(InvestigationCode)
  )

gc()

df_multiple_tests_summary <- df_multiple_tests_long |>
  group_by(id, invst_group) |>
  summarise(n_tests = n()) |>
  pivot_wider(
    names_from = "invst_group",
    values_from = "n_tests",
    names_prefix = "n_tests_"
  ) |>
  mutate(
    n_tests_Imaging = ifelse(is.na(n_tests_Imaging), 0, n_tests_Imaging),
    n_tests_Biochemistry = ifelse(is.na(n_tests_Biochemistry), 0, n_tests_Biochemistry),
    n_tests_Haematology = ifelse(is.na(n_tests_Haematology), 0, n_tests_Haematology),
    n_tests_Other = ifelse(is.na(n_tests_Other), 0, n_tests_Other)
  ) |>
  mutate(n_tests_all = n_tests_Imaging + n_tests_Biochemistry + n_tests_Haematology + n_tests_Other) |>
  select(-n_tests_NA)

gc()

df_multiple_tests <- df_multiple_tests_base |>
  left_join(
    df_multiple_tests_summary,
    join_by(id)
  )

saveRDS(df_multiple_tests, here("data_raw", "241125_df_multiple_tests.rds"))

rm(provider_sample, lkp_invst, lkp_test_ec_to_aea, lkp_invst)
rm(df_multiple_tests_base, df_multiple_tests_long, df_multiple_tests_summary)

gc()


# 4. CONSTRUCT AND SAVE PRE-PLOT DATAFRAMES -------------------------------------------------

# df_multiple_tests <- readRDS(here("data_raw", "241125_df_multiple_tests.rds"))

## a. overall: all attendances - number of tests (full distribution) --------
df_preplot_a_tests_per_attd <- df_multiple_tests |>
  group_by(fyear, n_tests_all) |>
  summarise(n_patients = n()) |>
  ungroup() |>
  group_by(fyear) |>
  mutate(
    p_patients = n_patients / sum(n_patients),
    mean_tests = sum(n_patients * n_tests_all) / sum(n_patients)
  )

## b. all attendances - number of tests grouped --------
df_preplot_b_tests_grp <- df_multiple_tests |>
  mutate(n_tests_all_grp = case_when(
    n_tests_all == 0 ~ "no tests",
    n_tests_all == 1 ~ "1 test",
    n_tests_all > 1 & n_tests_all <= 5 ~ "2-5 tests",
    n_tests_all >= 6 ~ "6+ tests",
    TRUE ~ NA_character_
  )) |>
  mutate(n_tests_all_grp = factor(
    n_tests_all_grp,
    levels = c("6+ tests", "2-5 tests", "1 test", "no tests")
  )) |>
  group_by(fyear, n_tests_all_grp) |>
  summarise(n_patients = n()) |>
  ungroup() |>
  group_by(fyear) |>
  mutate(p_patients = n_patients / sum(n_patients)) |>
  mutate(p_patients_adj = ifelse(n_tests_all_grp == "no tests", -p_patients, p_patients)) |>
  mutate(p_patients_label = paste0(as.character(round(p_patients * 100, 0)), "%"))

## c. split by admitted/non-admitted and injury/illness -------
df_preplot_c_tests_split_adm <- df_multiple_tests |>
  mutate(is_admitted = ifelse(disdest_grp == "admitted", "admitted", "not admitted")) |>
  mutate(inj_illness = case_when(
    is.na(inj_flag) ~ "illness",
    inj_flag == 1 ~ "injury",
    TRUE ~ "illness"
  )) |>
  mutate(n_tests_all_grp = case_when(
    n_tests_all == 0 ~ "no tests",
    n_tests_all == 1 ~ "1 test",
    n_tests_all <= 5 ~ "2-5 tests",
    TRUE ~ "6+ tests"
  )) |>
  mutate(n_tests_all_grp = factor(n_tests_all_grp,
                                  levels = c(
                                    "6+ tests", "2-5 tests",
                                    "1 test", "no tests"
                                  )
  )) |>
  group_by(fyear, n_tests_all_grp, is_admitted, inj_illness) |>
  summarise(n_patients = n()) |>
  ungroup() |>
  group_by(fyear, is_admitted, inj_illness) |>
  mutate(p_patients = n_patients / sum(n_patients)) |>
  mutate(p_patients_adj = ifelse(n_tests_all_grp == "no tests", -p_patients, p_patients)) |>
  mutate(p_patients_label = paste0(as.character(round(p_patients * 100, 0)), "%"))


## d. split by test type------
df_preplot_d_tests_split_type <- df_multiple_tests |>
  group_by(fyear, n_tests_Imaging) |>
  summarise(n_patients = n()) |>
  mutate(test_type = "Imaging") |>
  rename(n_tests = n_tests_Imaging) |>
  bind_rows(df_multiple_tests |>
              group_by(fyear, n_tests_Haematology) |>
              summarise(n_patients = n()) |>
              mutate(test_type = "Haematology") |>
              rename(n_tests = n_tests_Haematology)) |>
  bind_rows(df_multiple_tests |>
              group_by(fyear, n_tests_Biochemistry) |>
              summarise(n_patients = n()) |>
              mutate(test_type = "Biochemistry") |>
              rename(n_tests = n_tests_Biochemistry)) |>
  bind_rows(df_multiple_tests |>
              group_by(fyear, n_tests_Other) |>
              summarise(n_patients = n()) |>
              mutate(test_type = "Other tests") |>
              rename(n_tests = n_tests_Other)) |>
  mutate(n_tests_grp = case_when(
    n_tests == 0 ~ "no tests",
    n_tests == 1 ~ "1 test",
    n_tests >= 1 ~ "2+ tests"
  )) |>
  mutate(n_tests_grp = factor(n_tests_grp,
                              levels = c("2+ tests", "1 test", "no tests")
  )) |>
  group_by(fyear, test_type, n_tests_grp) |>
  summarise(n_patients = sum(n_patients)) |>
  ungroup() |>
  group_by(fyear, test_type) |>
  mutate(p_patients = n_patients / sum(n_patients)) |>
  mutate(p_patients_adj = ifelse(n_tests_grp == "no tests", -p_patients, p_patients)) |>
  mutate(p_patients_label = paste0(as.character(round(p_patients * 100, 0)), "%"))

## e. time overall ----------------
df_preplot_e_tests_vs_time <- df_multiple_tests |>
  mutate(n_tests_trunc = ifelse(n_tests_all >= 12, 12, n_tests_all)) |>
  group_by(fyear, n_tests_trunc) |>
  summarise(
    median_dur_assess_depart = median(dur_assess_depart),
    n_patients = n()
  )

## f. split by admitted/non-admitted and injury/illness -----
df_preplot_f_tests_vs_time_split_adm <- df_multiple_tests |>
  mutate(is_admitted = ifelse(disdest_grp == "admitted", "admitted", "not admitted")) |>
  mutate(inj_illness = case_when(
    is.na(inj_flag) ~ "illness",
    inj_flag == 1 ~ "injury",
    TRUE ~ "illness"
  )) |>
  mutate(n_tests_trunc = ifelse(n_tests_all >= 12, 12, n_tests_all)) |>
  group_by(fyear, n_tests_trunc, is_admitted, inj_illness) |>
  summarise(
    median_dur_assess_depart = median(dur_assess_depart),
    n_patients = n()
  )


## g. split by test type -----
df_preplot_g_tests_vs_time_split_type <- df_multiple_tests |>
  mutate(n_tests_trunc = ifelse(n_tests_Biochemistry >= 5, 5, n_tests_Biochemistry)) |>
  group_by(fyear, n_tests_trunc) |>
  summarise(
    median_dur_assess_depart = median(dur_assess_depart),
    n_patients = n()
  ) |>
  mutate(test_type = "Biochemistry") |>
  bind_rows(df_multiple_tests |>
              mutate(n_tests_trunc = ifelse(n_tests_Haematology >= 5, 5, n_tests_Haematology)) |>
              group_by(fyear, n_tests_trunc) |>
              summarise(
                median_dur_assess_depart = median(dur_assess_depart),
                n_patients = n()
              ) |>
              mutate(test_type = "Haematology")) |>
  bind_rows(df_multiple_tests |>
              mutate(n_tests_trunc = ifelse(n_tests_Imaging >= 5, 5, n_tests_Imaging)) |>
              group_by(fyear, n_tests_trunc) |>
              summarise(
                median_dur_assess_depart = median(dur_assess_depart),
                n_patients = n()
              ) |>
              mutate(test_type = "Imaging")) |>
  bind_rows(df_multiple_tests |>
              mutate(n_tests_trunc = ifelse(n_tests_Other >= 5, 5, n_tests_Other)) |>
              group_by(fyear, n_tests_trunc) |>
              summarise(
                median_dur_assess_depart = median(dur_assess_depart),
                n_patients = n()
              ) |>
              mutate(test_type = "Other tests"))


# 5. SAVE PRE-PLOT DATAFRAMES ---------------------------------------------

ls(pattern = "df_preplot") |>
  enframe(name = NULL) |>
  rename(obj_name = value) |>
  mutate(data = map(obj_name, ~ get(.))) |>
  saveRDS(here("data", "df_nested_preplots_multi_tests.rds"))


# 6. EDA looking at unusual shape of curve -------
# (time vs number of tests) for admitted patients in 2023/24

df_multiple_tests |>
  filter(disdest_grp == "admitted") |>
  # filter(is.na(inj_flag) | inj_flag == 0) |>
  # filter(inj_flag == 1) |>
  filter(n_tests_all == 2) |>
  group_by(fyear, n_tests_Imaging) |>
  summarise(n = n()) |>
  mutate(p = n / sum(n))

# of those admitted following 2 (and only 2 tests) in 2019/20 - only about a third (36.0%) had an imaging investigation
# of those admitted following 2 (and only 2 tests) in 2023/24 - about two thirds (63.7%) had an imaging investigation
# imaging takes longer than other tests
