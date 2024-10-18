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


# 1 load ecds data ----

# source("create_study_dataset.R")

pared_provider_sample <- 
  open_dataset(here("data", "pared_provider_sample.parquet")) |> 
  collect() |> 
  # ADD ASSESSMENT TO CONCLUSION DURATION (AND FLAG FOR ANOMALIES):
  mutate(dur_assess_concl = dur_arr_concl - as.numeric(difftime(dttm_arr, dttm_assess, units = "mins"))) |>
  mutate(flag_odd_time = if_else(dttm_arr > dttm_assess, 1, 0)) |>
  mutate(flag_odd_time = if_else(dur_assess_concl < 0, 1, flag_odd_time)) |>
  # NOTE: WE MAY WANT TO BE EVEN MORE CONSERVATIVE HERE:
  mutate(flag_odd_time = if_else(dur_arr_concl >= 96*60, 1, flag_odd_time))


gc()


# note the next sections (1.1 to 1.10) are taken from the model_odds.R script
# ultimately will be deleted and replaced by loading of the odds models

# 2 create investigation lookup ----------------------------------------------------

tb_ref_invst <- readRDS(here("data", "lkp_invst.RDS"))

lkp_test_ec_to_aea <- readRDS(here("data", "lkp_test_ec_to_aea.RDS"))

lkp_invst <- tb_ref_invst |>
  select(InvestigationKey, InvestigationCode, InvestigationDescription) |>
  # # REMOVE NO INVESTIGATION FROM SEARCH LIST
  # (NOTE: ITS PRESENCE DOESN'T INDICATE NO INVESTIGATION):
  filter(InvestigationKey != 98) |>
  collect() |> 
  left_join(lkp_test_ec_to_aea |> 
              select(InvestigationKey, invst_group),
            join_by(InvestigationKey))


# 3 create dataframe for analysis ----

df_multiple_tests_base <- pared_provider_sample |>
  mutate(id = row_number()) |>   
  select(id,
         fyear, arr_mode, acuity, acuity_desc, chief_comp_grp, inj_flag, disdest_grp,
         dttm_arr, dttm_assess, dttm_depart,
         dur_arr_assess, dur_arr_concl, duration_ed,
         dur_assess_concl, flag_odd_time,
         dttm_arr,
         Der_EC_Investigation_All) |> 
  # note filter removes 7815 cases outof c3m (0.3%)  
  filter(flag_odd_time != 1) 


df_multiple_tests_long <- df_multiple_tests_base |> 
  select(id, Der_EC_Investigation_All) |> 
  separate_longer_delim(Der_EC_Investigation_All, delim = ", ") |> 
  rename(InvestigationCode = Der_EC_Investigation_All) |> 
  mutate(InvestigationCode = as.integer(InvestigationCode)) |> 
  left_join(lkp_invst,
            join_by(InvestigationCode))

gc()


df_multiple_tests_summary <-  df_multiple_tests_long |> 
  group_by(id, invst_group) |> 
  summarise(n_tests = n()) |> 
  pivot_wider(names_from = 'invst_group',
              values_from = 'n_tests',
              names_prefix = 'n_tests_') |> 
  mutate(n_tests_Imaging = ifelse(is.na(n_tests_Imaging), 0, n_tests_Imaging),
         n_tests_Biochemistry = ifelse(is.na(n_tests_Biochemistry), 0, n_tests_Biochemistry),
         n_tests_Haematology = ifelse(is.na(n_tests_Haematology), 0, n_tests_Haematology),
         n_tests_Other = ifelse(is.na(n_tests_Other), 0, n_tests_Other)) |> 
  mutate(n_tests_all = n_tests_Imaging + n_tests_Biochemistry + n_tests_Haematology + n_tests_Other) |> 
  select(-n_tests_NA)
  


gc()

df_multiple_tests <- df_multiple_tests_base |> 
  left_join(df_multiple_tests_summary,
            join_by(id))

saveRDS(df_multiple_tests, here('data', 'df_multiple_tests.RDS'))

rm(pared_provider_sample, lkp_invst, lkp_test_ec_to_aea, tb_ref_invst)
rm(df_multiple_tests_base, df_multiple_tests_long, df_multiple_tests_summary)

gc()


# 4 visualise number of tests per patient ----
df_multiple_tests <- readRDS(here('data', 'df_multiple_tests.RDS'))


df_multiple_tests |> 
  group_by(fyear, n_tests_all) |> 
  summarise(n_patients = n()) |> 
  ungroup() |> 
  group_by(fyear) |> 
  mutate(p_patients = n_patients / sum(n_patients),
         mean_tests = sum(n_patients * n_tests_all) / sum(n_patients)) |> 
  ggplot() +
  geom_vline(aes(xintercept = mean_tests, colour = fyear), linetype = 'dashed') +
  geom_line(aes(x = n_tests_all, y = p_patients, colour = fyear)) +
  geom_point(aes(x = n_tests_all, y = p_patients, colour = fyear)) +
  scale_y_continuous(name = 'proportion of patients',
                     label = label_percent(accuracy = 1)) +
  scale_x_continuous(name = 'number of tests') 


df_multiple_tests |> 
  mutate(is_admitted = ifelse(disdest_grp == "admitted", "admitted", "not admitted")) |> 
  mutate(inj_illness = case_when(is.na(inj_flag) ~ "illness",
                                 inj_flag == 1 ~ "injury", 
                                 TRUE ~ "illness")) |> 
  group_by(fyear, n_tests_all, is_admitted, inj_illness) |> 
  summarise(n_patients = n()) |> 
  ungroup() |> 
  group_by(fyear, is_admitted, inj_illness) |> 
  mutate(p_patients = n_patients / sum(n_patients),
         mean_tests = sum(n_patients * n_tests_all) / sum(n_patients)) |> 
  ggplot() +
  geom_vline(aes(xintercept = mean_tests, colour = fyear), linetype = 'dashed') +
  geom_line(aes(x = n_tests_all, y = p_patients, colour = fyear)) +
  geom_point(aes(x = n_tests_all, y = p_patients, colour = fyear)) +
  scale_y_continuous(name = 'proportion of patients',
                     label = label_percent(accuracy = 1)) +
  scale_x_continuous(name = 'number of tests') +
  facet_grid(cols = vars(is_admitted), rows = vars(inj_illness))


df_multiple_tests |> 
  mutate(is_admitted = ifelse(disdest_grp == "admitted", "admitted", "not admitted")) |> 
  mutate(inj_illness = case_when(is.na(inj_flag) ~ "illness",
                                 inj_flag == 1 ~ "injury", 
                                 TRUE ~ "illness")) |> 
  mutate(n_tests_all_grp = case_when(n_tests_all == 0 ~ 'no tests',
                                     n_tests_all == 1 ~ '1 test',
                                     n_tests_all <= 5 ~ '2-5 tests',
                                     TRUE ~ '6+ tests')) |> 
  mutate(n_tests_all_grp = factor(n_tests_all_grp,
                                  levels = c('6+ tests', '2-5 tests',
                                             '1 test', 'no tests'))) |>
  group_by(fyear, n_tests_all_grp, is_admitted, inj_illness) |> 
  summarise(n_patients = n()) |> 
  ungroup() |> 
  group_by(fyear, is_admitted, inj_illness) |> 
  mutate(p_patients = n_patients / sum(n_patients)) |> 
  mutate(p_patients_adj = ifelse(n_tests_all_grp == 'no tests', -p_patients, p_patients)) |> 
  mutate(p_patients_label = paste0(as.character(round(p_patients*100, 0)), '%')) |> 
  ggplot() +
  geom_hline(aes(yintercept = 0), colour = 'grey') +
  geom_col(aes(x = fyear, y = p_patients_adj, fill = n_tests_all_grp),
           position = position_stack()) +
  geom_text(aes(x = fyear, y = p_patients_adj, group = n_tests_all_grp, label = p_patients_label),
           position = position_stack(vjust = 0.5)) +
  facet_grid(cols = vars(is_admitted), rows = vars(inj_illness)) +
  scale_fill_manual(values = c('red', 'orange', 'yellow', 'grey')) +
  scale_y_continuous(name = 'proportion of patients',
                     label = label_percent(accuracy = 1)) +
  scale_x_discrete(name = 'financial year') +
  theme(legend.title = element_blank())






df_multiple_tests |> 
  group_by(fyear, n_tests_Imaging) |> 
  summarise(n_patients = n()) |> 
  mutate(p_patients = n_patients / sum(n_patients)) |> 
  mutate(test_type = 'Imaging') |> 
  rename(n_tests = n_tests_Imaging) |> 
  bind_rows(df_multiple_tests |> 
              group_by(fyear, n_tests_Haematology) |> 
              summarise(n_patients = n()) |> 
              mutate(p_patients = n_patients / sum(n_patients)) |> 
              mutate(test_type = 'Haematology') |> 
              rename(n_tests = n_tests_Haematology) ) |> 
  bind_rows(df_multiple_tests |> 
              group_by(fyear, n_tests_Biochemistry) |> 
              summarise(n_patients = n()) |> 
              mutate(p_patients = n_patients / sum(n_patients)) |> 
              mutate(test_type = 'Biochemistry') |> 
              rename(n_tests = n_tests_Biochemistry) ) |> 
  bind_rows(df_multiple_tests |> 
              group_by(fyear, n_tests_Other) |> 
              summarise(n_patients = n()) |> 
              mutate(p_patients = n_patients / sum(n_patients)) |> 
              mutate(test_type = 'Other') |> 
              rename(n_tests = n_tests_Other))|> 
  ggplot() +
  geom_line(aes(x = n_tests, y = p_patients, colour = fyear)) +
  geom_point(aes(x = n_tests, y = p_patients, colour = fyear)) +
  scale_y_continuous(name = 'proportion of patients',
                     label = label_percent(accuracy = 1)) +
  scale_x_continuous(name = 'number of tests') +
  facet_wrap(vars(test_type))
  
  


df_multiple_tests |> 
  group_by(fyear, n_tests_Imaging) |> 
  summarise(n_patients = n()) |> 
  mutate(test_type = 'Imaging') |> 
  rename(n_tests = n_tests_Imaging) |> 
  bind_rows(df_multiple_tests |> 
              group_by(fyear, n_tests_Haematology) |> 
              summarise(n_patients = n()) |> 
              mutate(test_type = 'Haematology') |> 
              rename(n_tests = n_tests_Haematology) ) |> 
  bind_rows(df_multiple_tests |> 
              group_by(fyear, n_tests_Biochemistry) |> 
              summarise(n_patients = n()) |> 
              mutate(test_type = 'Biochemistry') |> 
              rename(n_tests = n_tests_Biochemistry) ) |> 
  bind_rows(df_multiple_tests |> 
              group_by(fyear, n_tests_Other) |> 
              summarise(n_patients = n()) |> 
              mutate(test_type = 'Other tests') |> 
              rename(n_tests = n_tests_Other)) |> 
  mutate(n_tests_grp = case_when(n_tests == 0 ~ 'no tests',
                                     n_tests == 1 ~ '1 test',
                                     n_tests >= 1 ~ '2+ tests')) |> 
  mutate(n_tests_grp = factor(n_tests_grp,
                                  levels = c('2+ tests','1 test', 'no tests'))) |>
  group_by(fyear, test_type, n_tests_grp) |> 
  summarise(n_patients = sum(n_patients)) |> 
  ungroup() |> 
  group_by(fyear, test_type) |> 
  mutate(p_patients = n_patients / sum(n_patients)) |> 
  mutate(p_patients_adj = ifelse(n_tests_grp == 'no tests', -p_patients, p_patients)) |> 
  mutate(p_patients_label = paste0(as.character(round(p_patients*100, 0)), '%')) |> 
  ggplot() +
  geom_hline(aes(yintercept = 0), colour = 'grey') +
  geom_col(aes(x = fyear, y = p_patients_adj, fill = n_tests_grp),
           position = position_stack()) +
  geom_text(aes(x = fyear, y = p_patients_adj, group = n_tests_grp, label = p_patients_label),
            position = position_stack(vjust = 0.5)) +
  facet_wrap(vars(test_type)) +
  scale_fill_manual(values = c('orange', 'yellow', 'grey')) +
  scale_y_continuous(name = 'proportion of patients',
                     #label = label_percent(accuracy = 1),
                     breaks = c(-0.5, -0.25, 0, 0.25, 0.5),
                     labels = c('50%', '25%', '0%', '25%', '50%')) +
  scale_x_discrete(name = 'financial year') +
  theme(legend.title = element_blank())
  



df_multiple_tests |> 
  mutate(is_admitted = ifelse(disdest_grp == "admitted", "admitted", "not admitted")) |> 
  group_by(fyear, is_admitted, n_tests_Imaging) |> 
  summarise(n_patients = n()) |> 
  mutate(test_type = 'Imaging') |> 
  rename(n_tests = n_tests_Imaging) |> 
  bind_rows(df_multiple_tests |> 
              mutate(is_admitted = ifelse(disdest_grp == "admitted", "admitted", "not admitted")) |> 
              group_by(fyear, is_admitted, n_tests_Haematology) |> 
              summarise(n_patients = n()) |> 
              mutate(test_type = 'Haematology') |> 
              rename(n_tests = n_tests_Haematology) ) |> 
  bind_rows(df_multiple_tests |> 
              mutate(is_admitted = ifelse(disdest_grp == "admitted", "admitted", "not admitted")) |> 
              group_by(fyear, is_admitted, n_tests_Biochemistry) |> 
              summarise(n_patients = n()) |> 
              mutate(test_type = 'Biochemistry') |> 
              rename(n_tests = n_tests_Biochemistry) ) |> 
  bind_rows(df_multiple_tests |> 
              mutate(is_admitted = ifelse(disdest_grp == "admitted", "admitted", "not admitted")) |> 
              group_by(fyear, is_admitted, n_tests_Other) |> 
              summarise(n_patients = n()) |> 
              mutate(test_type = 'Other tests') |> 
              rename(n_tests = n_tests_Other)) |> 
  mutate(n_tests_grp = case_when(n_tests == 0 ~ 'no tests',
                                 n_tests == 1 ~ '1 test',
                                 n_tests >= 1 ~ '2+ tests')) |> 
  mutate(n_tests_grp = factor(n_tests_grp,
                              levels = c('2+ tests','1 test', 'no tests'))) |>
  group_by(fyear, is_admitted, test_type, n_tests_grp) |> 
  summarise(n_patients = sum(n_patients)) |> 
  ungroup() |> 
  group_by(fyear, is_admitted, test_type) |> 
  mutate(p_patients = n_patients / sum(n_patients)) |> 
  mutate(p_patients_adj = ifelse(n_tests_grp == 'no tests', -p_patients, p_patients)) |> 
  mutate(p_patients_label = paste0(as.character(round(p_patients*100, 0)), '%')) |> 
  ggplot() +
  geom_hline(aes(yintercept = 0), colour = 'grey') +
  geom_col(aes(x = fyear, y = p_patients_adj, fill = n_tests_grp),
           position = position_stack()) +
  geom_text(aes(x = fyear, y = p_patients_adj, group = n_tests_grp, label = p_patients_label),
            position = position_stack(vjust = 0.5)) +
  facet_grid(rows =vars(test_type),
             cols = vars(is_admitted)) +
  scale_fill_manual(values = c('orange', 'yellow', 'grey')) +
  scale_y_continuous(name = 'proportion of patients',
                     #label = label_percent(accuracy = 1),
                     breaks = c(-0.5, -0.25, 0, 0.25, 0.5),
                     labels = c('50%', '25%', '0%', '25%', '50%')) +
  scale_x_discrete(name = 'financial year') +
  theme(legend.title = element_blank())



# 5 visualise time in ED ----

df_multiple_tests |> 
  mutate(is_admitted = ifelse(disdest_grp == "admitted", "admitted", "not admitted")) |> 
  group_by(fyear, is_admitted) |> 
  summarise(arrival_to_assessment = mean(dur_arr_assess, na.rm = TRUE),
            assessment_to_conclusion = mean(dur_assess_concl, na.rm = TRUE),
            conclsuion_to_departure = mean(duration_ed - dur_arr_concl, na.rm = TRUE)) |> 
  pivot_longer(cols = 3:5,
               names_to = 'component_of_stay',
               values_to = 'mean_duration') |> 
  mutate(component_of_stay = factor(component_of_stay,
                                    levels = c('arrival_to_assessment', 
                                               'assessment_to_conclusion', 
                                               'conclsuion_to_departure'))) |> 
  ggplot() +
  geom_col(aes(x = mean_duration, y = fyear, fill = component_of_stay),
           position = position_stack(reverse = TRUE)) +
  facet_wrap(vars(is_admitted)) +
  scale_y_discrete(name = '',
                   limits = rev)




# 6 visualise time vs tests ----



df_multiple_tests |> 
  mutate(is_admitted = ifelse(disdest_grp == "admitted", "admitted", "not admitted")) |> 
  mutate(n_tests = n_tests_all - n_tests_Imaging) |> 
  group_by(fyear, n_tests, is_admitted) |> 
  summarise(mean_duration_ed = mean(duration_ed, na.rm = TRUE),
            mean_dur_assess_concl = mean(dur_assess_concl),
            n_patients = n()) |> 
  pivot_longer(cols = 4:5, 
               names_to = 'metric',
               values_to = 'duration') |> 
  ggplot() +
  geom_line(aes(x = n_tests, 
                y = duration, 
                colour = fyear)) +
  geom_point(aes(x = n_tests, 
                 y = duration, 
                 colour = fyear,
                 size = n_patients)) +
  facet_grid(rows = vars(metric), cols = vars(is_admitted)) +
  scale_y_continuous(name = 'mean duration (assessment to conclusion',
                     limits = c(0, NA_real_)) +
  scale_x_continuous(name = 'number of tests') 


# df_prep_binary |>  
#   mutate(n_tests = select(df_prep_binary, starts_with("invst_")) |>  rowSums()) |> 
#   mutate(n_tests_trunc = ifelse(n_tests >= 12, 12, n_tests)) |> 
#   mutate(is_admitted = ifelse(disdest_grp == "admitted", "admitted", "not admitted")) |> 
#   mutate(ed_duration_hrs = duration_ed / 60) |> 
#   group_by(fyear, is_admitted, n_tests_trunc) |> 
#   summarise(median_ed_duration_hrs = median(ed_duration_hrs, na.rm = TRUE),
#             n = n()) |> 
#   ggplot() +
#   geom_line(aes(x = n_tests_trunc, 
#                 y = median_ed_duration_hrs, 
#                 colour = fyear)) +
#   geom_point(aes(x = n_tests_trunc, 
#                  y = median_ed_duration_hrs, 
#                  colour = fyear, 
#                  size = n)) +
#   facet_wrap(vars(is_admitted)) +
#   scale_x_continuous(name = 'number of tests',
#                      limits = c(0, 12), 
#                      breaks = (0:12),
#                      labels = c('0', '1', '2', '3', '4', 
#                                 '5', '6', '7', '8', '9', 
#                                 '10', '11', '12+')) +
#   scale_y_continuous(name  = 'median time in ED (hrs)',
#                      limits = c(0, 12.5))
# 
# 
# df_prep_binary |>  
#   mutate(n_tests = select(df_prep_binary, starts_with("invst_")) |>  rowSums()) |> 
#   mutate(n_tests_trunc = ifelse(n_tests >= 12, 12, n_tests)) |> 
#   mutate(is_admitted = ifelse(disdest_grp == "admitted", "admitted", "not admitted")) |> 
#   mutate(inj_illness = ifelse(inj_flag == 1, "injury", "illness")) |> 
#   mutate(ed_duration_hrs = duration_ed / 60) |> 
#   group_by(fyear, inj_illness, n_tests_trunc) |> 
#   summarise(median_ed_duration_hrs = median(ed_duration_hrs, na.rm = TRUE),
#             n = n()) |> 
#   ggplot() +
#   geom_line(aes(x = n_tests_trunc, 
#                 y = median_ed_duration_hrs, 
#                 colour = fyear)) +
#   geom_point(aes(x = n_tests_trunc, 
#                  y = median_ed_duration_hrs, 
#                  colour = fyear, 
#                  size = n)) +
#   facet_wrap(vars(inj_illness)) +
#   scale_x_continuous(name = 'number of tests',
#                      limits = c(0, 12), 
#                      breaks = (0:12),
#                      labels = c('0', '1', '2', '3', '4', 
#                                 '5', '6', '7', '8', '9', 
#                                 '10', '11', '12+')) +
#   scale_y_continuous(name  = 'median time in ED (hrs)',
#                      limits = c(0, 12.5))
# 
# df_prep_binary |>  
#   mutate(n_tests = select(df_prep_binary, starts_with("invst_")) |>  rowSums()) |> 
#   mutate(n_tests_trunc = ifelse(n_tests >= 12, 12, n_tests)) |> 
#   mutate(is_admitted = ifelse(disdest_grp == "admitted", "admitted", "not admitted")) |> 
#   mutate(inj_illness = ifelse(inj_flag == 1, "injury", "illness")) |> 
#   mutate(ed_duration_hrs = duration_ed / 60) |> 
#   group_by(fyear, inj_illness, is_admitted, n_tests_trunc) |> 
#   summarise(median_ed_duration_hrs = median(ed_duration_hrs, na.rm = TRUE),
#             n = n()) |> 
#   ggplot() +
#   geom_line(aes(x = n_tests_trunc, 
#                 y = median_ed_duration_hrs, 
#                 colour = fyear)) +
#   geom_point(aes(x = n_tests_trunc, 
#                  y = median_ed_duration_hrs, 
#                  colour = fyear, 
#                  size = n)) +
#   facet_grid(rows = vars(inj_illness),
#              cols = vars(is_admitted)) +
#   scale_x_continuous(name = 'number of tests',
#                      limits = c(0, 12), 
#                      breaks = (0:12),
#                      labels = c('0', '1', '2', '3', '4', 
#                                 '5', '6', '7', '8', '9', 
#                                 '10', '11', '12+')) +
#   scale_y_continuous(name  = 'median time in ED (hrs)',
#                      limits = c(0, 12.5))
# 
# df_prep_binary |>  
#   mutate(n_tests = select(df_prep_binary, starts_with("invst_")) |>  rowSums()) |> 
#   mutate(n_tests_trunc = ifelse(n_tests >= 12, 12, n_tests)) |> 
#   mutate(is_admitted = ifelse(disdest_grp == "admitted", "admitted", "not admitted")) |> 
#   mutate(inj_illness = ifelse(inj_flag == 1, "injury", "illness")) |> 
#   mutate(ed_duration_hrs = duration_ed / 60) |> 
#   group_by(fyear, chief_comp_grp, n_tests_trunc) |> 
#   summarise(median_ed_duration_hrs = median(ed_duration_hrs, na.rm = TRUE),
#             n = n()) |> 
#   ggplot() +
#   geom_line(aes(x = n_tests_trunc, 
#                 y = median_ed_duration_hrs, 
#                 colour = fyear)) +
#   geom_point(aes(x = n_tests_trunc, 
#                  y = median_ed_duration_hrs, 
#                  colour = fyear, 
#                  size = n)) +
#   facet_wrap(vars(chief_comp_grp)) +
#   scale_x_continuous(name = 'number of tests',
#                      limits = c(0, 12), 
#                      breaks = (0:12),
#                      labels = c('0', '1', '2', '3', '4', 
#                                 '5', '6', '7', '8', '9', 
#                                 '10', '11', '12+')) +
#   scale_y_continuous(name  = 'median time in ED (hrs)',
#                      limits = c(0, 18))
# 
# 
# df_prep_binary |>  
#   mutate(n_tests = select(df_prep_binary, starts_with("invst_")) |>  rowSums()) |> 
#   mutate(n_tests_trunc = ifelse(n_tests >= 12, 12, n_tests)) |> 
#   mutate(is_admitted = ifelse(disdest_grp == "admitted", "admitted", "not admitted")) |> 
#   mutate(inj_illness = ifelse(inj_flag == 1, "injury", "illness")) |> 
#   mutate(ed_duration_hrs = duration_ed / 60) |> 
#   group_by(fyear, chief_comp_grp, is_admitted, n_tests_trunc) |> 
#   summarise(median_ed_duration_hrs = median(ed_duration_hrs, na.rm = TRUE),
#             n = n()) |> 
#   ggplot() +
#   geom_line(aes(x = n_tests_trunc, 
#                 y = median_ed_duration_hrs, 
#                 colour = fyear)) +
#   geom_point(aes(x = n_tests_trunc, 
#                  y = median_ed_duration_hrs, 
#                  colour = fyear, 
#                  size = n)) +
#   facet_grid(cols = vars(chief_comp_grp),
#              rows = vars(is_admitted)) +
#   scale_x_continuous(name = 'number of tests',
#                      limits = c(0, 12), 
#                      breaks = (0:12),
#                      labels = c('0', '1', '2', '3', '4', 
#                                 '5', '6', '7', '8', '9', 
#                                 '10', '11', '12+')) +
#   scale_y_continuous(name  = 'median time in ED (hrs)',
#                      limits = c(0, 18))
# 
# 
# 
# 
# df_prep_binary |>
#   select(1, 22:69) |> 
#   group_by(fyear) |>
#   summarise_all(sum) |> 
#   pivot_longer(cols = 2:49,
#                names_to = 'invst',
#                values_to = 'n') |> 
#   pivot_wider(names_from = 'fyear',
#               values_from = 'n') |> 
#   mutate(InvestigationKey = as.numeric(substr(invst, 7, 8))) |> 
#   left_join(lkp_invst, join_by(InvestigationKey)) |> 
#   mutate(abs_growth = `2023/24` - `2019/20`,
#          rel_growth = (`2023/24` / `2019/20`) - 1) |> 
#   select(c(6, 2, 3, 7, 8)) |> 
#   arrange(-abs_growth) |> 
#   print(n = 48)
#   