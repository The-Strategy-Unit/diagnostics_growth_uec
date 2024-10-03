# README
# (NCDR)
# Model odds of ED test 19/20 vs 23/24. 
# Work in progress 

# TODO POSSIBILITIES:
# - WORTH LOOKING AT ODDS OF GETTING *ANY* TEST (REGARDLESS OF TYPE)?
# - LIKELIHOOD OF TWO OR MORE TESTS OF SAME TYPE?
# - WE HAVE LOW EVENT RATES FOR SOME (UNGROUPED) TESTS - ADDRESS?
# - ... EG. WITH UNDERSAMPLING? OR WILL LARGER SAMPLE SIZES SOLVE THIS?

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

source("create_study_dataset.R")

## 1. LOOKUP FOR INVESTIGATIONS ----------------------------------------------------

con_nhse_reference <- dbConnect(
  odbc::odbc(),
  Driver = "SQL Server",
  Server = "PRODNHSESQL101",
  Database = "NHSE_Reference",
  Trusted_Connection = "True"
)

tb_ref_invst <- tbl(con_nhse_reference, in_schema("dbo", "tbl_Ref_DataDic_ECDS_Investigation"))

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

# 2. ODDS DF ----------------------------------------------------------

df_odds <- pared_provider_sample |>
  select(-matches("disdest|^diag|Diagnosis|age_grp|lso|dur|dttm|^reg|^lacd|is_mar|^time_|n_cmr"), dttm_arr)

# 3. MISSING VALUES - DELETE ---------------------------------------------------

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


# 4. RE-ENGINEER CHIEF COMPLAINT -----------------------------------------
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

# 5. SAMPLE 10% FOR TRIAL --------------------------------------------------------------

# SHORTER MODEL RUN TIME FOR FASTER FEEDBACK
set.seed(1822)
df_odds_sample <- df_odds_na_rm |>
  slice_sample(prop = 0.1)


# 6. OUTCOME VARIABLES ------------------------------------------

## a. create ----------------------------------------------------

# NOTE: COUNTING INSTANCES HERE TO KEEP OPTIONS OPEN.
# BUT LATER ADJUSTED TO BINARY (TEST Y/N).
list_invst_counts <- imap(vec_invst_codes_ec, function(x, y) {
  df_odds_sample %>%
    transmute({{ y }} := str_count(Der_EC_Investigation_All, str_c("^", x, "| ", x, "|,", x)))
})

df_odds_invst <- list_invst_counts |>
  reduce(bind_cols) |>
  bind_cols(df_odds_sample, y = _)


## b. eda groups ----------------------------------------------------------------
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
## c. * plot outcomes * -------------------------------------------------------------------
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


# 7. CREATE TIME-RELATED FEATURES -------------------------------

df_odds_invst_time <- df_odds_invst |>
  mutate(month = month(dttm_arr)) |>
  mutate(wkday = wday(dttm_arr, label = T, week_start = 1)) |>
  mutate(hour = hour(dttm_arr)) |>
  mutate(is_winter = if_else(month %in% c(12, 1:3), 1, 0)) |>
  mutate(is_wkend = if_else(wkday %in% c("Sat", "Sun"), 1, 0)) |>
  mutate(is_night = if_else(hour %in% c(0:5, 23), 1, 0))


# 8. FINAL DATA PREP ------------------------------------------------------

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


# ~~~~~~~~~ -------------------------------------------------------------------

# THE FUNCTIONAL PROGRAMMING FOR MULTIPLE MODELS ---------------------
# (SINGLE MODEL ALSO DEMO'ED)


# CREATE DATA FRAME FOR FUNCTIONAL PROGRAMMING:
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

# PREVIEW:
df2


## a. demo for one test ----------------------------------------------------
# NOT TESTED:

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


##  b. Models for 10 tests --------------------------------------------------------

df3 <- df2 |>
  mutate(model = map(data, function(df) {
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
      data = df
    )
  }))
# ON 300K RECORDS THIS CHUNK WILL TAKE ~ 8 MINS PER MODEL

# df3 |>
#   select(-data) |>
#   saveRDS("from_ncdr_model_odds_ex_241002_top_10.rds")

gc()

# PLOT ODDS ---------------------------------------------------------------

df3 |>
  mutate(odds = map_dbl(model, function(df) {
    df |>
      broom::tidy(parametric = TRUE) |>
      mutate(odds = exp(estimate), .before = estimate) |>
      filter(term == "fyear2023/24") |>
      pull(odds)
  })) |> 
  ggplot() +
  geom_col(aes(reorder(InvestigationDescription, odds), odds)) +
  theme_bw() +
  theme_minimal() +
  coord_flip() +
  theme(
    axis.title.y = element_blank(),
    axis.text = element_text(size = 12)
  )
