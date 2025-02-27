# README
# Model odds of ED test in 23/24 vs 19/20 (NCDR)

# TODO POSSIBILITIES:
# - WORTH LOOKING AT ODDS OF GETTING *ANY* TEST (REGARDLESS OF TYPE)?
# - LIKELIHOOD OF TWO OR MORE TESTS OF SAME TYPE?
# - WE HAVE LOW EVENT RATES FOR SOME (UNGROUPED) TESTS - ADDRESS?
# - EG. WITH UNDERSAMPLING? OR WILL LARGER SAMPLE SIZES SOLVE THIS?

library("DBI")
library("here")
library("purrr")
library("furrr") 
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

source(here("R", "create_study_dataset.R"))
source(here("R","lkp_tests_ec_to_aea.R"))


# 1. ODDS DF ----------------------------------------------------------

df_odds <- provider_sample |> 
  # REMOVE MARCH FROM BOTH YEARS TO MINIMISE COVID BIAS:
  filter(is_march == 0) |> 
  select(-matches("disdest|^diag|Diagnosis|age_grp|lso|dur|dttm|^reg|^lacd|is_mar|^time_|^n_"), dttm_arr)

# 2. MISSING VALUES - DELETE ---------------------------------------------------

df_odds_na_rm <- df_odds |>
  filter(!is.na(acuity)) |>
  filter(!is.na(chief_comp_grp)) |>
  # ALTERNATIVELY, COULD ASSUME NA IS NO INVESTIGATION? (AND IMPUTE)
  filter(!is.na(Der_EC_Investigation_All)) |>
  filter(!is.na(age)) |>
  # NOT USING IN FINAL MODEL:
  # filter(!is.na(imd_dec)) |> 
  # filter(!is.na(inj_flag)) |>
  identity()


# # BY DELETING ALL MISSING VALUES FROM KEY FIELDS,
# nrow(df_odds) -nrow(df_odds_na_rm)
# WE ARE LOSING 112,751 RECORDS (3.7%).
# df_odds |> nrow() - df_odds_na_rm |> nrow()
# 1 - df_odds_na_rm |> nrow() / df_odds |> nrow()


# 3. RE-ENGINEER VARS --------------------------------------------

## a. NEW CHIEF COMPLAINT --------------------------------------------
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

df_odds_fe1 <- df_odds_na_rm |> 
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

## b. ATTENDANCE (REFERRAL) SOURCE ----------------------------------------------------

df_odds_fe2 <- df_odds_fe1 |> 
  mutate(refer_sorc = str_remove_all(att_source_desc, "[:punct:]")) |>
  mutate(refer_sorc = str_remove_all(refer_sorc, " procedure| finding| situation")) |> 
  mutate(refer_sorc = str_remove_all(refer_sorc, "Referred by |Referral by ")) |> 
  mutate(refer_sorc = str_remove_all(refer_sorc, " accident and emergency department")) |> 
  # distinct(attsorc) |> 
  # print(n=40)
  mutate(refer_sorc = case_when(
    refer_sorc == "Selfreferral to" ~ "self", 
    refer_sorc == "member of Primary Health Care Team" ~ "primary_care",
    refer_sorc == "urgent care service" ~ "urgent_care",
    T ~ "other"
  )) |> 
  mutate(refer_sorc = as.factor(refer_sorc)) |>
  # SET REFERENCE LEVEL AS SELF-REFERRAL:
  mutate(refer_sorc = fct_relevel(refer_sorc, "self")) |> 
  select(-starts_with("att")) 


## c. TIME-RELATED VARS ----------------------------------------------------

df_odds_fe3 <- df_odds_fe2 |>
  mutate(month = month(dttm_arr)) |>
  mutate(wkday = wday(dttm_arr, label = T, week_start = 1)) |>
  # REMOVE THE ORDERING ON WEEKDAY:
  mutate(wkday = as.character(wkday)) |>
  mutate(hour = hour(dttm_arr)) |>
  mutate(is_winter = if_else(month %in% c(12, 1:3), 1, 0)) |>
  mutate(is_wkend = if_else(wkday %in% c("Sat", "Sun"), 1, 0)) |>
  # NIGHT = 8 HRS FROM 22:00-05:59:
  mutate(is_night = if_else(hour %in% c(0:5, 22:23), 1, 0)) |> 
  mutate(across(starts_with("is_"), ~ as.factor(.))) |>
  select(-dttm_arr)

# 4. SAMPLE 35% (OR > 1 MILLION RECORDS) ------------------------

set.seed(1822)
df_odds_sample <- df_odds_fe3 |>
  slice_sample(prop = 0.35)

gc()

# # SHORTER MODEL RUN TIME FOR FASTER FEEDBACK
# set.seed(1822)
# df_odds_sample <- df_odds_fe3 |>
#   slice_sample(prop = 0.17)
 
# gc()

# 5. OUTCOME VARIABLES ------------------------------------------

## a. CREATE ----------------------------------------------------

# PULL EC TYPE CODES:
vec_invst_codes_ec <- lkp_invst |>
  mutate(InvestigationCode = as.character(InvestigationCode)) |>
  mutate(InvestigationKey = str_c("invst_", InvestigationKey)) |>
  select(InvestigationKey, InvestigationCode) |>
  deframe()

# NOTE: COUNTING INSTANCES HERE TO KEEP OPTIONS OPEN.
# BUT LATER ADJUSTED TO BINARY (TEST Y/N).
list_invst_counts <- imap(vec_invst_codes_ec, function(x, y) {
  df_odds_sample %>%
    transmute({{ y }} := str_count(Der_EC_Investigation_All, str_c("^", x, "| ", x, "|,", x)))
})

df_odds_invst <- list_invst_counts |>
  reduce(bind_cols) |>
  bind_cols(df_odds_sample, y = _)

gc()

## ** b. eda groups ----------------------------------------------------------------

# FOR BINARY OUTCOME TEST/NOT:
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
## ** c. plot outcomes  -------------------------------------------------------------------
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


# 6. FINAL DATA PREP ------------------------------------------------------

df_prep_binary <- df_odds_invst |>
  ### REMOVE VARS THAT WON'T BE USED IN BASIC MODEL:
  select(-c(ethnic_grp, acuity_desc)) |>
  # SWITCH OUTCOMES TO BINARY:
  mutate(across(starts_with("invst_"), ~ if_else(. > 0, 1, 0))) 

glimpse(df_prep_binary)


# ~~~~~~~~~ -------------------------------------------------------------------

# 7. FUNCTIONAL PROGRAMMING SETUP (FOR MULTIPLE MODELS) ---------------------
# FOUR MODELS RUN IN PARALLEL USING FURRR PACKAGE

# (SINGLE MODEL ALSO DEMO'ED)

# LOOK AT THE TOP 32 TESTS BY FREQUENCY 
# (THERE COULD BE UP TO 4 "OTHER" GROUPS FOR LOW FREQ TESTS BASED ON THE
# FOUR INVESTIGATION CLASSES WE MAY USE.)

# CREATE DATA FRAME FOR FUNCTIONAL PROGRAMMING:
# (TOP 10 TESTS BY APPEARANCE)
df1 <- df_prep_binary |>
  summarise(across(starts_with("invst_"), ~ sum(.))) |>
  pivot_longer(cols = everything()) |>
  arrange(-value) |>
  mutate(id = row_number()) |> 
  # X to Y (OF TOP Z) BY APPEARANCES:
  slice(1:4) |>
  mutate(InvestigationKey = as.numeric(str_extract(name, "[:digit:]{2}"))) |>
  left_join(lkp_invst, join_by(InvestigationKey)) |>
  select(name, value, InvestigationDescription, id) |> 
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
      select(everything(), -matches("invst"), all_of(x)) |>
      mutate(across(all_of(x), ~ as.factor(.))) |>
      # TO AVOID COMPLICATION IN MODEL FORMULA:
      rename("invst" = x)
  }))

gc()
gc()

# PREVIEW:
df2


## ** a1. demo for one simple model -----------------------------------------------
# NOT TESTED:

# df2$data[[9]] # ROW 9 = CT. USE AS DATA ARGUMENT BELOW
# 
# demo_model <- 
#   mgcv::gam(
#     formula = invst ~
#       # VAR OF INTEREST:
#       fyear +
#       # DEMOGRAPHICS:
#       s(age) + sex + # imd_dec +
#       # CASE-MIX-RELATED:
#       arr_mode + acuity + chief_comp_desc +
#       # TIME-RELATED:
#       is_winter +
#       # PROVIDER (TODO: RANDOM EFFECT):
#       procode,
#     family = "binomial",
#     data = df2$data[[9]]
#   )
# 
# summary(demo_model)
# 
# demo_model |>
#   broom::tidy(parametric = TRUE) |>
#   mutate(odds = exp(estimate), .before = estimate) |>
#   mutate(across(where(is.numeric), ~ round(., 4))) |>
#   select(-std.error, statistic) |>
#   mutate(sig = case_when(
#     p.value < 0.05 & p.value > 0.01 ~ "*",
#     p.value < 0.01 & p.value > 0.001 ~ "**",
#     p.value < 0.001 ~ "***",
#     T ~ ""
#   )) |>
#   view("ct_model_results")
# 
# df2$data[[9]] |>
#   mutate(class_1 = predict(demo_model, newdata = df2$data[[9]], type = "response")) |> 
#   select(invst, class_1) |> 
#   mutate(class_0 = 1 - class_1, .before = class_1) |> 
#   yardstick::roc_auc(invst, class_0)
# 
# df2$data[[9]] |>
#   mutate(class_1 = predict(demo_model, newdata = df2$data[[9]], type = "response")) |> 
#   select(invst, class_1) |> 
#   mutate(class_0 = 1 - class_1, .before = class_1) |> 
#   mutate(predicted = as.factor(if_else(class_1 >= 0.4, 1, 0))) |> 
#   yardstick::metrics(invst, predicted)


## ** a2. demo with  final model spec -----------------------------------------------
# NOT TESTED:

# df2$data[[9]] # ROW 9 = CT. USE AS DATA ARGUMENT BELOW
# 
# tictoc::tic()
# demo_model_v2 <- 
#   mgcv::gam(
#     formula = invst ~
#       # VAR OF INTEREST:
#       fyear +
#       # DEMOGRAPHICS:
#       s(age, by = sex) + sex + # imd_dec +
#       # CASE-MIX-RELATED:
#       arr_mode + acuity + chief_comp_desc + refer_sorc +
#       # TIME-RELATED:
#       is_winter + is_wkend + is_night +
#       # PROVIDER (RANDOM INTERCEPT):
#       s(procode, bs = "re"),
#     family = "binomial",
#     method = "REML",
#     data = df2$data[[9]]
#   )
# tictoc::toc()
# # ~40 mins
# 
# summary(demo_model_v2)
# 
# demo_model_v2 |>
#   broom::tidy(parametric = TRUE) |>
#   mutate(odds = exp(estimate), .before = estimate) |>
#   mutate(across(where(is.numeric), ~ round(., 4))) |>
#   select(-std.error, statistic) |>
#   mutate(sig = case_when(
#     p.value < 0.05 & p.value > 0.01 ~ "*",
#     p.value < 0.01 & p.value > 0.001 ~ "**",
#     p.value < 0.001 ~ "***",
#     T ~ ""
#   )) |>
#   view("ct_model_results")
# 
# head(predict(demo_model_v2, newdata = df2$data[[9]], type = "response")) # ranefs on
# head(predict(demo_model_v2, newdata = df2$data[[9]], exclude = "s(procode)", type = "response")) # ranefs off
# 
# df2$data[[9]] |>
#   mutate(class_1 = predict(demo_model_v2, newdata = df2$data[[9]], type = "response")) |> 
#   select(invst, class_1) |> 
#   mutate(class_0 = 1 - class_1, .before = class_1) |> 
#   yardstick::roc_auc(invst, class_0)
# 
# df2$data[[9]] |>
#   mutate(class_1 = predict(demo_model_v2, newdata = df2$data[[9]], type = "response")) |> 
#   select(invst, class_1) |> 
#   mutate(class_0 = 1 - class_1, .before = class_1) |> 
#   mutate(predicted = as.factor(if_else(class_1 >= 0.38, 1, 0))) |> 
#   yardstick::metrics(invst, predicted)


# 8. MODELS FOR TESTS X TO Y --------------------------------------------------------

plan(multisession, workers = 4)

tictoc::tic()
df3 <- df2 |>
  mutate(model = future_map(data, function(df) {
    mgcv::gam(
      formula = invst ~
        # VAR OF INTEREST:
        fyear +
        # DEMOGRAPHICS:
        s(age, by = sex) + sex + # imd_dec +
        # CASE-MIX-RELATED:
        arr_mode + acuity + chief_comp_desc + refer_sorc +
        # TIME-RELATED:
        is_winter + is_wkend + is_night +
        # PROVIDER (RANDOM INTERCEPT):
        s(procode, bs = "re"),
      family = "binomial",
      method = "REML",
      data = df
    )
  })) 
tictoc::toc()

gc()

# 9. SAVE MODEL RESULTS -------------------------------------------------------------

df3 %>% 
  saveRDS(str_c("models_odds_top_", min(.$id), "to", max(.$id), ".rds"))

