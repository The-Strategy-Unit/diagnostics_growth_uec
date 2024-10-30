# README
# Consider impact of multiple tests on time in ED
# ignore casemix - show unadjusted impact


# 0 set-up ----
library("DBI")
library("here")
library("purrr")
library("arrow")
library("dplyr")
library("tidyr")
library("dbplyr")
library("tibble")
library("forcats")
library("janitor")
library("ggplot2")
library("stringr")
library("lubridate")
library("scales")


# 1. LOAD ECDS DATA -------------------------------------------------------

# source("create_study_dataset.R")

provider_sample <-
  open_dataset(here("data_raw", "provider_sample.parquet")) |>
  collect() |>
  # ADD ASSESSMENT TO CONCLUSION DURATION (AND FLAG FOR ANOMALIES):
  mutate(dur_assess_depart = duration_ed - as.numeric(difftime(dttm_arr, dttm_assess, units = "mins"))) |>
  mutate(flag_odd_time = if_else(dttm_arr > dttm_assess, 1, 0)) |>
  mutate(flag_odd_time = if_else(dur_assess_depart < 0, 1, flag_odd_time)) |>
  # NOTE: WE MAY WANT TO BE EVEN MORE CONSERVATIVE HERE:
  mutate(flag_odd_time = if_else(dur_assess_depart >= 96 * 60, 1, flag_odd_time))


gc()


# 2 create investigation lookup ----------------------------------------------------

tb_ref_invst <- readRDS(here("data_raw", "lkp_invst.RDS"))

lkp_test_ec_to_aea <- readRDS(here("data_raw", "lkp_test_ec_to_aea.RDS"))

lkp_invst <- tb_ref_invst |>
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
  select(-matches("disdest|^diag|Diagnosis|age_grp|lso|dur|^reg|^lacd|is_mar|^time_|^n_"), dur_assess_depart,  disdest_grp) |>
  # select(id,
  #        fyear, arr_mode, acuity, acuity_desc, chief_comp_grp, inj_flag, disdest_grp,
  #        dttm_arr, dttm_assess, dttm_depart,
  #        dur_arr_assess, dur_arr_concl, duration_ed,
  #        dur_assess_depart, flag_odd_time,
  #        dttm_arr,
  #        Der_EC_Investigation_All) |>
  # note filter removes 7813 cases outof c3m (0.3%)
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

saveRDS(df_multiple_tests, here("data_raw", "df_multiple_tests.rds"))

rm(provider_sample, lkp_invst, lkp_test_ec_to_aea, tb_ref_invst)
rm(df_multiple_tests_base, df_multiple_tests_long, df_multiple_tests_summary)

gc()


# 4. VISUALISE NUMBER OF TESTS PER ATTENDANCE -----------------------------
# 4 visualise number of tests per patient ----
df_multiple_tests <- readRDS(here("data_raw", "df_multiple_tests.rds"))


# TODO NUMBER OF TESTS PER ATTENDANCE???
# all attendances - number of tests (full distribution)
df_multiple_tests |>
  group_by(fyear, n_tests_all) |>
  summarise(n_patients = n()) |>
  ungroup() |>
  group_by(fyear) |>
  mutate(
    p_patients = n_patients / sum(n_patients),
    mean_tests = sum(n_patients * n_tests_all) / sum(n_patients)
  ) |>
  ggplot() +
  # theme_bw()+
  theme_minimal() +
  geom_vline(aes(xintercept = mean_tests, colour = fyear), linetype = "dashed") +
  geom_line(aes(x = n_tests_all, y = p_patients, colour = fyear)) +
  geom_point(aes(x = n_tests_all, y = p_patients, colour = fyear)) +
  scale_y_continuous(
    name = "proportion of patients",
    label = label_percent(accuracy = 1)
  ) +
  scale_x_continuous(name = "number of tests") +
  labs(
    title = "Number of tests per patient",
    subtitle = "Selected providers | Apr19-Feb20 & Apr23-Feb24",
    caption = "dashed lines indicate mean number of tests"
  )

# TODO Y AXIS
# all attendances - number of tests grouped
df_multiple_tests |>
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
  mutate(p_patients_label = paste0(as.character(round(p_patients * 100, 0)), "%")) |>
  #
  ggplot() +
  geom_hline(aes(yintercept = 0), colour = "grey") +
  geom_col(
    aes(fyear, p_patients_adj, fill = n_tests_all_grp),
    position = position_stack()
  ) +
  # geom_col(
  #   aes(fyear, rev(p_patients), colour = n_tests_all_grp),
  #   fill = "transparent",
  #   colour = "grey20",
  #   lty = "dashed",
  #   position = position_stack()
  # ) +
  geom_text(aes(x = fyear, y = p_patients_adj, group = n_tests_all_grp, label = p_patients_label),
    position = position_stack(vjust = 0.5)
  ) +
  scale_fill_manual(values = c( "red", "orange", "yellow", "grey")) +
  # scale_colour_manual(values = c("red", "orange", "yellow", "grey")) +
  scale_y_continuous(
    name = "Proportion of patients",
    label = label_percent(accuracy = 1)
  ) +
  scale_x_discrete(name = "Financial year") +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    panel.grid = element_blank(),
    axis.text.y = element_blank()
  ) +
  labs(
    title = "Number of tests per patient",
    subtitle = "Selected providers | Apr19-Feb20 & Apr23-Feb24"
  )



# split by admitted/non-admitted and injury/illness
df_multiple_tests |>
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
  mutate(p_patients_label = paste0(as.character(round(p_patients * 100, 0)), "%")) |>
  ggplot() +
  geom_hline(aes(yintercept = 0), colour = "grey") +
  geom_col(aes(x = fyear, y = p_patients_adj, fill = n_tests_all_grp),
    position = position_stack()
  ) +
  geom_text(
    aes(x = fyear, y = p_patients_adj, group = n_tests_all_grp, label = p_patients_label),
    position = position_stack(vjust = 0.5), 
    size = 2
  ) +
  facet_grid(cols = vars(is_admitted), rows = vars(inj_illness)) +
  scale_fill_manual(values = c("red", "orange", "yellow", "grey")) +
  scale_y_continuous(
    name = "proportion of patients",
    label = label_percent(accuracy = 1)
  ) +
  scale_x_discrete(name = "financial year") +
  theme(legend.title = element_blank()) +
  theme_bw()+
  theme(
    legend.title = element_blank(),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  labs(
    title = "Number of tests per patient by disposal and presentation type",
    subtitle = "Selected providers | Apr19-Feb20 & Apr23-Feb24"
  )




# split by test type
df_multiple_tests |>
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
  mutate(p_patients_label = paste0(as.character(round(p_patients * 100, 0)), "%")) |>
  ggplot() +
  geom_hline(aes(yintercept = 0), colour = "grey") +
  geom_col(aes(x = fyear, y = p_patients_adj, fill = n_tests_grp),
    position = position_stack()
  ) +
  geom_text(aes(x = fyear, y = p_patients_adj, group = n_tests_grp, label = p_patients_label),
    position = position_stack(vjust = 0.5)
  ) +
  facet_wrap(vars(test_type)) +
  scale_fill_manual(values = c("orange", "yellow", "grey")) +
  scale_y_continuous(
    name = "proportion of patients",
    # label = label_percent(accuracy = 1),
    breaks = c(-0.5, -0.25, 0, 0.25, 0.5),
    labels = c("50%", "25%", "0%", "25%", "50%")
  ) +
  scale_x_discrete(name = "financial year") +
  theme_bw()+
  theme(
    legend.title = element_blank(),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  labs(
    title = "Number of tests per patient by test type",
    subtitle = "Selected providers | Apr19-Feb20 & Apr23-Feb24"
  )

# 5 visualise time vs tests ----

# all attendances -----
df_multiple_tests |>
  mutate(n_tests_trunc = ifelse(n_tests_all >= 12, 12, n_tests_all)) |>
  group_by(fyear, n_tests_trunc) |>
  summarise(
    median_dur_assess_depart = median(dur_assess_depart),
    n_patients = n()
  ) |>
  ggplot() +
  theme_minimal()+
  theme(
    axis.ticks = element_blank(),
    panel.grid.minor.x = element_blank()
    )+
  geom_line(aes(
    x = n_tests_trunc,
    y = median_dur_assess_depart,
    colour = fyear
  )) +
  geom_point(aes(
    x = n_tests_trunc,
    y = median_dur_assess_depart,
    colour = fyear,
    size = n_patients
  )) +
  scale_y_continuous(
    name = "Median time in ED (assessment to departure)",
    limits = c(0, NA_real_)
  ) +
  scale_x_continuous(
    name = "Number of tests",
    limits = c(0, 12),
    breaks = (0:12),
    labels = c(
      "0", "1", "2", "3", "4",
      "5", "6", "7", "8", "9",
      "10", "11", "12+"
    )
  ) +
  scale_size_continuous(name = "number of patients") +
  scale_colour_discrete(name = "financial year") +
  labs(
    title = "Median time in ED by number of tests",
    subtitle = "Selected providers | Apr19-Feb20 & Apr23-Feb24"
  )


# split by admitted/non-admitted and injury/illness -----
df_multiple_tests |>
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
  ) |>
  ggplot() +
  # theme_minimal()+
  theme_bw()+
  theme(
    axis.ticks = element_blank(),
    panel.grid.minor.x = element_blank()
  )+
  geom_line(aes(
    x = n_tests_trunc,
    y = median_dur_assess_depart,
    colour = fyear
  )) +
  geom_point(aes(
    x = n_tests_trunc,
    y = median_dur_assess_depart,
    colour = fyear,
    size = n_patients
  )) +
  facet_grid(
    cols = vars(is_admitted),
    rows = vars(inj_illness)
  ) +
  scale_y_continuous(
    name = "Median time in ED (assessment to departure)",
    limits = c(0, NA_real_)
  ) +
  scale_x_continuous(
    name = "number of tests",
    limits = c(0, 12),
    breaks = (0:12),
    labels = c(
      "0", "1", "2", "3", "4",
      "5", "6", "7", "8", "9",
      "10", "11", "12+"
    )
  ) +
  scale_size_continuous(name = "number of patients") +
  scale_colour_discrete(name = "financial year") +
  labs(
    title = "Median time in ED and number of tests by dispsosal and presentation type",
    subtitle = "Selected providers | Apr19-Feb20 & Apr23-Feb24"
  )


# split by test type-----
df_multiple_tests |>
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
    mutate(test_type = "Other tests")) |>
  ggplot() +
  theme_bw()+
  theme(
    axis.ticks = element_blank(),
    panel.grid.minor.x = element_blank()
  )+
  geom_line(aes(
    x = n_tests_trunc,
    y = median_dur_assess_depart,
    colour = fyear
  )) +
  geom_point(aes(
    x = n_tests_trunc,
    y = median_dur_assess_depart,
    colour = fyear,
    size = n_patients
  )) +
  facet_wrap(vars(test_type)) +
  scale_y_continuous(
    name = "Median time in ED (assessment to departure)",
    limits = c(0, NA_real_)
  ) +
  scale_x_continuous(
    name = "number of tests",
    limits = c(0, 5),
    breaks = (0:5),
    labels = c(
      "0", "1", "2", "3", "4",
      "5+"
    )
  ) +
  scale_size_continuous(name = "Number of patients") +
  scale_colour_discrete(name = "Financial year") +
  labs(
    title = "Median time and number of tests in ED by test type",
    subtitle = "Selected providers | Apr19-Feb20 & Apr23-Feb24"
  )




# 6 explore unusual shape of curve (time vs number of tests) for admitted patients in 2023/24 --

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
