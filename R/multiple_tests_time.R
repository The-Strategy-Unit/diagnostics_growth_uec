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


# 1 load data and create odds model ----

# source("create_study_dataset.R")

pared_provider_sample <- 
  open_dataset(here("data", "pared_provider_sample.parquet")) |> 
  collect() 

gc()


# note the next sections (1.1 to 1.10) are taken from the model_odds.R script
# ultimately will be deleted and replaced by loading of the odds models

## 1.1 LOOKUP FOR INVESTIGATIONS ----------------------------------------------------

tb_ref_invst <- readRDS(here("data", "lkp_invst.RDS"))

lkp_invst <- tb_ref_invst |>
  select(InvestigationKey, InvestigationCode, InvestigationDescription) |>
  # # REMOVE NO INVESTIGATION FROM SEARCH LIST
  # (NOTE: ITS PRESENCE DOESN'T INDICATE NO INVESTIGATION):
  filter(InvestigationKey != 98) |>
  collect()

# PULL EC TYPE CODES:
vec_invst_codes_ec <- lkp_invst |>
  mutate(InvestigationCode = as.character(InvestigationCode)) |>
  mutate(InvestigationKey = str_c("invst_", InvestigationKey)) |>
  select(InvestigationKey, InvestigationCode) |>
  deframe()


## 1.2. ODDS DF ----------------------------------------------------------
df_odds <- pared_provider_sample |>
  select(-matches("^diag|Diagnosis|age_grp|lso|dttm|^reg|^lacd|is_mar|^time_|n_cmr"), dttm_arr)


## 1.3. MISSING VALUES - DELETE ---------------------------------------------------

df_odds_na_rm <- df_odds |>
  filter(!is.na(acuity)) |>
  filter(!is.na(chief_comp_grp)) |>
  filter(!is.na(inj_flag)) |>
  # TODO COULD ASSUME NA IS NO INVESTIGATION? (I.E. IMPUTATION)
  filter(!is.na(Der_EC_Investigation_All)) |>
  filter(!is.na(age)) |>
  filter(!is.na(imd_dec))

# # BY DELETING ALL MISSING VALUES FROM KEY FIELDS,
# # WE ARE LOSING 113,240 RECORDS (3.6%).
# df_odds |> nrow() - df_odds_na_rm |> nrow()
# 1 - df_odds_na_rm |> nrow() / df_odds |> nrow()

## 1.4. RE-ENGINEER CHIEF COMPLAINT -----------------------------------------
# AN IMPORTANT VAR AND FAR MORE INFORMATIVE THAN GROUPED COMPLAINT:

lkp_chief_comp_small <- df_odds_na_rm |>
  count(chief_comp_desc, chief_comp_grp, sort = T) |>
  mutate(p = round(n / sum(n), 4) * 100) |>
  ungroup() |>
  filter(p < 0.2) |>
  count(chief_comp_grp, chief_comp_desc, wt = p, sort = T) |>
  # count(chief_comp_grp, wt = n, sort = T) |>
  # GROUP THOSE GROUPS WITH THE LOWEST OCCURENCES:
  mutate(chief_comp_grp = if_else(
    chief_comp_grp %in% c(
      "Not applicable to child terms",
      "Neurological"
    ),
    "Other",
    chief_comp_grp
  )) |>
  mutate(chief_comp_desc_small = str_c("Other_", chief_comp_grp)) |>
  select(-c(chief_comp_grp, n))


## 1.5. SAMPLE 10% FOR TRIAL --------------------------------------------------------------

## SHORTER MODEL RUN TIME FOR FASTER FEEDBACK
set.seed(1822)
df_odds_sample <- df_odds_na_rm |>
  slice_sample(prop = 0.1)


## 1.6. OUTCOME VARIABLES ------------------------------------------

### a. create ----------------------------------------------------

# NOTE: COUNTING INSTANCES HERE TO KEEP OPTIONS OPEN.
# BUT LATER ADJUSTED TO BINARY (TEST Y/N).
list_invst_counts <- imap(vec_invst_codes_ec, function(x, y) {
  df_odds_sample %>%
    transmute({{ y }} := str_count(Der_EC_Investigation_All, str_c("^", x, "| ", x, "|,", x)))
})

df_odds_invst <- list_invst_counts |>
  reduce(bind_cols) |>
  bind_cols(df_odds_sample, y = _)


### b. eda groups ----------------------------------------------------------------
#
# # FOR BINARY OUTCOME TEST/NOT:
# outcome_binary_counts <- df_odds_invst |>
#   mutate(across(starts_with("invst_"), ~ if_else(. > 0 , 1, 0))) |>
#   group_by(fyear) |>
#   summarise(across(starts_with("invst_"), ~sum(.))) |>
#   ungroup() |>
#   pivot_longer(cols = starts_with("invst_")) |>
#   arrange(value) |>
#   mutate(name = as.numeric(str_remove_all(name, "invst_"))) |>
#   right_join(lkp_invst, join_by(name == InvestigationKey)) |>
#   mutate(InvestigationDescription = str_remove_all(InvestigationDescription, " \\(procedure\\)| \\(situation\\)"))
#
# vec_group_other <- outcome_binary_counts |>
#   group_by(across(-c(fyear, value))) |>
#   mutate(value = sum(value)) |>
#   ungroup() |>
#   distinct(InvestigationDescription, InvestigationCode, value) |>
#   mutate(p_cases = value/nrow(df_odds_invst) ) |>
#   # 1 IN 250 CUTOFF WILL ENSURE > 1000 INSTANCES FOR 10% SAMPLE
#   filter(value/nrow(df_odds_invst) < 0.004) |>
#   arrange(-value) |>
#   mutate(InvestigationCode = as.character(InvestigationCode)) |>
#   pull(InvestigationCode)
#
#
### c. * plot outcomes * -------------------------------------------------------------------
#
# preplot_outcome_counts <- outcome_binary_counts |>
#   mutate(InvestigationDescription = case_when(
#     InvestigationCode %in% vec_group_other ~ "Other (<1/250)",
#     TRUE ~ InvestigationDescription
#   )) |>
#   group_by(fyear, InvestigationDescription) |>
#   summarise(value = sum(value), name = mean(name)) |>
#   ungroup()
#
# left_join(
#   df_odds_invst |> count(fyear),
#   preplot_outcome_counts |>
#   filter(name == 73),
#   join_by(fyear)
# ) |>
#   mutate(p_att = value/n)

# preplot_outcome_counts|>
#   saveRDS("from_ncdr_mod_odds_ex_240924_preplot_outcome_counts.rds")
#   # left_join(
#   #   df_odds_invst |>
#   #     count(fyear, name = "n_att"),
#   #   join_by(fyear)
#   #   ) |>
#   ggplot()+
#   geom_col(
#     aes(
#       reorder(str_c(name, "_", InvestigationDescription), value),
#       value/nrow(df_odds_invst),
#       fill = fyear
#     ),
#     position = "dodge"
#   )+
#   theme_minimal()+
#   # scale_y_continuous(labels = scales::comma)+
#   scale_y_continuous(labels = scales::percent, limits = c(0, .26))+
#   coord_flip()+
#   # 1 IN 3000 CUTOFF WILL ENSURE ~ 1000 INSTANCES FOR 10% SAMPLE
#   # LINE IS AT 0.3% OF CASES:
#   geom_hline(yintercept = 1/250, lty = "dashed")+
#   labs(y = "% of all attendances in the dataset")+
#   theme(
#     axis.title.y = element_blank()
#   ) |>
#   # facet_wrap(vars(fyear))+
#   NULL


## 1.7. CREATE TIME-RELATED FEATURES -------------------------------

df_odds_invst_time <- df_odds_invst |>
  mutate(month = month(dttm_arr)) |>
  mutate(wkday = wday(dttm_arr, label = T, week_start = 1)) |>
  mutate(hour = hour(dttm_arr)) |>
  mutate(is_winter = if_else(month %in% c(12, 1:3), 1, 0)) |>
  mutate(is_wkend = if_else(wkday %in% c("Sat", "Sun"), 1, 0)) |>
  mutate(is_night = if_else(hour %in% c(0:5, 23), 1, 0))


## 1.8. FINAL DATA PREP ------------------------------------------------------

df_prep_binary <- df_odds_invst_time |>
  ### REMOVE VARS THAT WON'T BE USED IN BASIC MODEL:
  select(-c(ethnic_grp, acuity_desc, starts_with("att"))) |>
  # REMOVE THE ORDERING ON WEEKDAY:
  mutate(wkday = as.character(wkday)) |>
  # SWITCH OUTCOMES TO BINARY:
  mutate(across(starts_with("invst_"), ~ if_else(. > 0, 1, 0))) |>
  # REPLACE CHIEF COMPLAINT WITH ENGINEERED VERSION:
  left_join(lkp_chief_comp_small, join_by(chief_comp_desc)) |>
  mutate(chief_comp_desc = if_else(
    !is.na(chief_comp_desc_small),
    chief_comp_desc_small,
    chief_comp_desc
  )) |>
  select(-chief_comp_desc_small) |>
  mutate(chief_comp_desc = as.factor(chief_comp_desc)) |>
  # SET REFERENCE LEVEL AS MOST FREQ COMPLAINT:
  mutate(chief_comp_desc = fct_relevel(chief_comp_desc, "Chest pain (finding)"))


glimpse(df_prep_binary)



  
df_prep_binary |>  
  mutate(n_tests = select(df_prep_binary, starts_with("invst_")) |>  rowSums()) |> 
  mutate(n_tests_trunc = ifelse(n_tests >= 12, 12, n_tests)) |> 
  mutate(is_admitted = ifelse(disdest_grp == "admitted", "admitted", "not admitted")) |> 
  mutate(ed_duration_hrs = duration_ed / 60) |> 
  group_by(fyear, is_admitted, n_tests_trunc) |> 
  summarise(median_ed_duration_hrs = median(ed_duration_hrs, na.rm = TRUE),
            n = n()) |> 
  ggplot() +
  geom_line(aes(x = n_tests_trunc, 
                y = median_ed_duration_hrs, 
                colour = fyear)) +
  geom_point(aes(x = n_tests_trunc, 
                 y = median_ed_duration_hrs, 
                 colour = fyear, 
                 size = n)) +
  facet_wrap(vars(is_admitted)) +
  scale_x_continuous(name = 'number of tests',
                     limits = c(0, 12), 
                     breaks = (0:12),
                     labels = c('0', '1', '2', '3', '4', 
                                '5', '6', '7', '8', '9', 
                                '10', '11', '12+')) +
  scale_y_continuous(name  = 'median time in ED (hrs)',
                     limits = c(0, 12.5))


df_prep_binary |>  
  mutate(n_tests = select(df_prep_binary, starts_with("invst_")) |>  rowSums()) |> 
  mutate(n_tests_trunc = ifelse(n_tests >= 12, 12, n_tests)) |> 
  mutate(is_admitted = ifelse(disdest_grp == "admitted", "admitted", "not admitted")) |> 
  mutate(inj_illness = ifelse(inj_flag == 1, "injury", "illness")) |> 
  mutate(ed_duration_hrs = duration_ed / 60) |> 
  group_by(fyear, inj_illness, n_tests_trunc) |> 
  summarise(median_ed_duration_hrs = median(ed_duration_hrs, na.rm = TRUE),
            n = n()) |> 
  ggplot() +
  geom_line(aes(x = n_tests_trunc, 
                y = median_ed_duration_hrs, 
                colour = fyear)) +
  geom_point(aes(x = n_tests_trunc, 
                 y = median_ed_duration_hrs, 
                 colour = fyear, 
                 size = n)) +
  facet_wrap(vars(inj_illness)) +
  scale_x_continuous(name = 'number of tests',
                     limits = c(0, 12), 
                     breaks = (0:12),
                     labels = c('0', '1', '2', '3', '4', 
                                '5', '6', '7', '8', '9', 
                                '10', '11', '12+')) +
  scale_y_continuous(name  = 'median time in ED (hrs)',
                     limits = c(0, 12.5))

df_prep_binary |>  
  mutate(n_tests = select(df_prep_binary, starts_with("invst_")) |>  rowSums()) |> 
  mutate(n_tests_trunc = ifelse(n_tests >= 12, 12, n_tests)) |> 
  mutate(is_admitted = ifelse(disdest_grp == "admitted", "admitted", "not admitted")) |> 
  mutate(inj_illness = ifelse(inj_flag == 1, "injury", "illness")) |> 
  mutate(ed_duration_hrs = duration_ed / 60) |> 
  group_by(fyear, inj_illness, is_admitted, n_tests_trunc) |> 
  summarise(median_ed_duration_hrs = median(ed_duration_hrs, na.rm = TRUE),
            n = n()) |> 
  ggplot() +
  geom_line(aes(x = n_tests_trunc, 
                y = median_ed_duration_hrs, 
                colour = fyear)) +
  geom_point(aes(x = n_tests_trunc, 
                 y = median_ed_duration_hrs, 
                 colour = fyear, 
                 size = n)) +
  facet_grid(rows = vars(inj_illness),
             cols = vars(is_admitted)) +
  scale_x_continuous(name = 'number of tests',
                     limits = c(0, 12), 
                     breaks = (0:12),
                     labels = c('0', '1', '2', '3', '4', 
                                '5', '6', '7', '8', '9', 
                                '10', '11', '12+')) +
  scale_y_continuous(name  = 'median time in ED (hrs)',
                     limits = c(0, 12.5))

df_prep_binary |>  
  mutate(n_tests = select(df_prep_binary, starts_with("invst_")) |>  rowSums()) |> 
  mutate(n_tests_trunc = ifelse(n_tests >= 12, 12, n_tests)) |> 
  mutate(is_admitted = ifelse(disdest_grp == "admitted", "admitted", "not admitted")) |> 
  mutate(inj_illness = ifelse(inj_flag == 1, "injury", "illness")) |> 
  mutate(ed_duration_hrs = duration_ed / 60) |> 
  group_by(fyear, chief_comp_grp, n_tests_trunc) |> 
  summarise(median_ed_duration_hrs = median(ed_duration_hrs, na.rm = TRUE),
            n = n()) |> 
  ggplot() +
  geom_line(aes(x = n_tests_trunc, 
                y = median_ed_duration_hrs, 
                colour = fyear)) +
  geom_point(aes(x = n_tests_trunc, 
                 y = median_ed_duration_hrs, 
                 colour = fyear, 
                 size = n)) +
  facet_wrap(vars(chief_comp_grp)) +
  scale_x_continuous(name = 'number of tests',
                     limits = c(0, 12), 
                     breaks = (0:12),
                     labels = c('0', '1', '2', '3', '4', 
                                '5', '6', '7', '8', '9', 
                                '10', '11', '12+')) +
  scale_y_continuous(name  = 'median time in ED (hrs)',
                     limits = c(0, 18))


df_prep_binary |>  
  mutate(n_tests = select(df_prep_binary, starts_with("invst_")) |>  rowSums()) |> 
  mutate(n_tests_trunc = ifelse(n_tests >= 12, 12, n_tests)) |> 
  mutate(is_admitted = ifelse(disdest_grp == "admitted", "admitted", "not admitted")) |> 
  mutate(inj_illness = ifelse(inj_flag == 1, "injury", "illness")) |> 
  mutate(ed_duration_hrs = duration_ed / 60) |> 
  group_by(fyear, chief_comp_grp, is_admitted, n_tests_trunc) |> 
  summarise(median_ed_duration_hrs = median(ed_duration_hrs, na.rm = TRUE),
            n = n()) |> 
  ggplot() +
  geom_line(aes(x = n_tests_trunc, 
                y = median_ed_duration_hrs, 
                colour = fyear)) +
  geom_point(aes(x = n_tests_trunc, 
                 y = median_ed_duration_hrs, 
                 colour = fyear, 
                 size = n)) +
  facet_grid(cols = vars(chief_comp_grp),
             rows = vars(is_admitted)) +
  scale_x_continuous(name = 'number of tests',
                     limits = c(0, 12), 
                     breaks = (0:12),
                     labels = c('0', '1', '2', '3', '4', 
                                '5', '6', '7', '8', '9', 
                                '10', '11', '12+')) +
  scale_y_continuous(name  = 'median time in ED (hrs)',
                     limits = c(0, 18))




df_prep_binary |>
  select(1, 22:69) |> 
  group_by(fyear) |>
  summarise_all(sum) |> 
  pivot_longer(cols = 2:49,
               names_to = 'invst',
               values_to = 'n') |> 
  pivot_wider(names_from = 'fyear',
              values_from = 'n') |> 
  mutate(InvestigationKey = as.numeric(substr(invst, 7, 8))) |> 
  left_join(lkp_invst, join_by(InvestigationKey)) |> 
  mutate(abs_growth = `2023/24` - `2019/20`,
         rel_growth = (`2023/24` / `2019/20`) - 1) |> 
  select(c(6, 2, 3, 7, 8)) |> 
  arrange(-abs_growth) |> 
  print(n = 48)
  