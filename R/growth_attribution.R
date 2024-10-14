# This file sets out a method to attribute the total growth (G) in tests into:
# (A) demand growth
# (B) changes in casemix
# (C) changes in practice

# Note that (3) is the residual growth, i.e. total growth minus growth 
# attributable to (1) and (2).

# Start with a single test

# The process is:
# 0 set-up
# 1 load data and create/load odds model 
# 2 establish total growth in tests (G)
# 3 establish change in attendances
# 4 use this to derive demand growth (A)
# 5 predict tests in yr2, as if in yr1 (i.e. under practice conditions from yr1)
#         use this and (A) to derive casemix growth (B)
# 6 anything left is practice growth (C)



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
  select(-matches("disdest|^diag|Diagnosis|age_grp|lso|dur|dttm|^reg|^lacd|is_mar|^time_|n_cmr"), dttm_arr)


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

## 1.9 CREATE DATA FRAME FOR FUNCTIONAL PROGRAMMING: --------
# (TOP 10 TESTS BY APPEARANCE)
df1 <- df_prep_binary |>
  summarise(across(starts_with("invst_"), ~ sum(.))) |>
  pivot_longer(cols = everything()) |>
  arrange(-value) |>
  # TOP 10 BY APPEARANCE:
  slice(1:10) |>
  mutate(InvestigationKey = as.numeric(str_extract(name, "[:digit:]{2}"))) |>
  left_join(lkp_invst, join_by(InvestigationKey)) |>
  select(name, value, InvestigationDescription) |> 
  # REMOVE SUPERFLUOUS TEXT:
  mutate(InvestigationDescription = str_remove_all(InvestigationDescription, "[:punct:]")) |>
  mutate(InvestigationDescription = str_remove_all(InvestigationDescription, " procedure")) |> 
  relocate(InvestigationDescription, .after = name)


# NOTE!!
# IF YOU NEED TO PREVIEW df2 (BELOW) BEST DONE IN CONSOLE WINDOW
# RATHER THAN WITH view()

# ADD REQUIRED DATA IN LIST COLUMN (AND MANIPULATE):
df2 <- df1 |>
  mutate(data = list(df_prep_binary)) |>
  mutate(data = map2(data, name, function(df, x) {
    df |>
      select(everything(), -matches("nv|dttm|^n_diag"), all_of(x)) |>
      mutate(
        across(
          c(month, wkday, hour, starts_with("is_"), all_of(x)),
          ~ as.factor(.)
        )
      ) |>
      # TO AVOID COMPLICATION IN MODEL FORMULA:
      rename("invst" = x)
  }))

gc()
gc()

## a. demo for one test ----------------------------------------------------
# NOT TESTED:

## 1.10  create demo model ----
df2$data[[9]] # ROW 9 = CT. USE AS DATA ARGUMENT BELOW

demo_model <- 
  mgcv::gam(
    formula = invst ~
      # VAR OF INTEREST:
      fyear +
      # DEMOGRAPHICS:
      s(age) + sex + # imd_dec +
      # CASE-MIX-RELATED:
      arr_mode + acuity + chief_comp_desc +
      # TIME-RELATED:
      is_winter +
      # PROVIDER (TODO: RANDOM EFFECT):
      procode,
    family = "binomial",
    data = df2$data[[9]]
  )

summary(demo_model)

demo_model |>
  broom::tidy(parametric = TRUE) |>
  mutate(odds = exp(estimate), .before = estimate) |>
  mutate(across(where(is.numeric), ~ round(., 4))) |>
  select(-std.error, statistic) |>
  mutate(sig = case_when(
    p.value < 0.05 & p.value > 0.01 ~ "*",
    p.value < 0.01 & p.value > 0.001 ~ "**",
    p.value < 0.001 ~ "***",
    T ~ ""
  )) |>
  view("ct_model_results")


# 2 establish absolute growth in tests (G) ----

test_growth_G <- df2$data[[9]] |> 
  filter(invst == 1) |> 
  group_by(fyear) |> 
  summarise(tests = n()) |> 
  pivot_wider(names_from = "fyear",
              values_from = "tests",
              names_prefix = "tests") |> 
  mutate(G = `tests2023/24` - `tests2019/20`)






# 3 establish relative growth in attendances -----
rel_atts_growth <- df2$data[[9]] |> 
  group_by(fyear) |> 
  summarise(atts = n()) |> 
  pivot_wider(names_from = "fyear",
              values_from = "atts",
              names_prefix = "atts") |> 
  mutate(atts_growth_rel = (`atts2023/24` / `atts2019/20`) - 1) |> 
  select(atts_growth_rel) |> 
  pull()
  
  

# 4 use this to derive demand growth (A) ----
# if tests had grown in line with growth in attendances

test_growth_GA <- test_growth_G |> 
  mutate(A = `tests2019/20` * rel_atts_growth)
  
  
# 5 predict tests in yr2, as if in yr1 ----
# (i.e. under practice conditions from yr1)
# use this and (A) to derive casemix growth (B)

act_df <- df2$data[[9]] |> 
  filter(fyear == "2023/24")

pred_df <- df2$data[[9]] |> 
  filter(fyear == "2023/24") |> 
  mutate(fyear = factor("2019/20", levels = c("2019/20", "2023/24")))


test_growth_GAB <- test_growth_GA |> 
  mutate(B = sum(predict(demo_model, newdata = act_df, type = "response")) -
           sum(predict(demo_model, newdata = pred_df, type = "response")) - 
           A)

# 6 anything left is practice growth (C) ----
test_growth_GABC <- test_growth_GAB |> 
  mutate(C = G - A - B)



#### sonme checks to understand growth

df2$data[[9]] |> 
  group_by(fyear, sex) |> 
  summarise(atts = n()) |> 
  pivot_wider(names_from = "fyear",
              values_from = "atts",
              names_prefix = "atts") |> 
  mutate(growth = (`atts2023/24` / `atts2019/20`) - 1)


df2$data[[9]] |> 
  mutate(age75plus = ifelse(age >= 75, 1, 0)) |> 
  group_by(fyear, age75plus) |> 
  summarise(atts = n()) |> 
  pivot_wider(names_from = "fyear",
              values_from = "atts",
              names_prefix = "atts") |> 
  mutate(growth = (`atts2023/24` / `atts2019/20`) - 1)

df2$data[[9]] |> 
  group_by(fyear, imd_dec) |> 
  summarise(atts = n()) |> 
  pivot_wider(names_from = "fyear",
              values_from = "atts",
              names_prefix = "atts") |> 
  mutate(growth = (`atts2023/24` / `atts2019/20`) - 1)


df2$data[[9]] |> 
  group_by(fyear, arr_mode) |> 
  summarise(atts = n()) |> 
  pivot_wider(names_from = "fyear",
              values_from = "atts",
              names_prefix = "atts") |> 
  mutate(growth = (`atts2023/24` / `atts2019/20`) - 1)


df2$data[[9]] |> 
  group_by(fyear, acuity) |> 
  summarise(atts = n()) |> 
  pivot_wider(names_from = "fyear",
              values_from = "atts",
              names_prefix = "atts") |> 
  mutate(growth = (`atts2023/24` / `atts2019/20`) - 1)

df2$data[[9]] |> 
  group_by(fyear, inj_flag) |> 
  summarise(atts = n()) |> 
  pivot_wider(names_from = "fyear",
              values_from = "atts",
              names_prefix = "atts") |> 
  mutate(growth = (`atts2023/24` / `atts2019/20`) - 1)


df2$data[[9]] |> 
  group_by(fyear, chief_comp_grp) |> 
  summarise(atts = n()) |> 
  pivot_wider(names_from = "fyear",
              values_from = "atts",
              names_prefix = "atts") |> 
  mutate(growth = (`atts2023/24` / `atts2019/20`) - 1)


df2$data[[9]] |> 
  group_by(fyear, chief_comp_desc) |> 
  summarise(atts = n()) |> 
  pivot_wider(names_from = "fyear",
              values_from = "atts",
              names_prefix = "atts") |> 
  mutate(growth = (`atts2023/24` / `atts2019/20`) - 1)



df2$data[[9]] |> 
  group_by(fyear, chief_comp_desc) |> 
  summarise(atts = n()) |> 
  pivot_wider(names_from = "fyear",
              values_from = "atts",
              names_prefix = "atts") |> 
  mutate(growth = (`atts2023/24` / `atts2019/20`) - 1)

df2$data[[9]] |> 
  group_by(fyear, is_winter) |> 
  summarise(atts = n()) |> 
  pivot_wider(names_from = "fyear",
              values_from = "atts",
              names_prefix = "atts") |> 
  mutate(growth = (`atts2023/24` / `atts2019/20`) - 1)

df2$data[[9]] |> 
  group_by(fyear, is_wkend) |> 
  summarise(atts = n()) |> 
  pivot_wider(names_from = "fyear",
              values_from = "atts",
              names_prefix = "atts") |> 
  mutate(growth = (`atts2023/24` / `atts2019/20`) - 1)

df2$data[[9]] |> 
  group_by(fyear, is_night) |> 
  summarise(atts = n()) |> 
  pivot_wider(names_from = "fyear",
              values_from = "atts",
              names_prefix = "atts") |> 
  mutate(growth = (`atts2023/24` / `atts2019/20`) - 1)

df2$data[[9]] |> 
  group_by(fyear, procode) |> 
  summarise(atts = n()) |> 
  pivot_wider(names_from = "fyear",
              values_from = "atts",
              names_prefix = "atts") |> 
  mutate(growth = (`atts2023/24` / `atts2019/20`) - 1)





