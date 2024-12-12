# README
# HOW MUCH TIME DOES A TEST ADD TO PATIENT'S STAY IN THE ED?

library("here")
library("purrr")
library("arrow")
library("dplyr")
library("tidyr")
library("tibble")
library("forcats")
library("janitor")
library("ggplot2")
library("stringr")
library("sandwich")
library("lubridate")


# -------------------------------------------------------------------------


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

lkp_invst <- readRDS(here("reference", "lkp_invst.rds"))


# 1. DURATION DF ----------------------------------------------------------
# WHAT INFO DO WE NOT NEED / NOT HAVE AT THE TIME OF ASSESSMENT?
df_duration <- provider_sample |>
  filter(fyear == "2023/24") |>
  filter(flag_odd_time != 1) |>
  select(-matches("disdest|age_grp|lso|^reg|^lacd|is_march|fyear|^n_|^flag_odd"), n_diag_ec) |>
  # BASED ON EDA OF DURATIONS, EXCLUDE OVER 30 HOURS AS MAY OVERLY INFLUENCE MODEL:
  filter(dur_assess_depart < 30 * 60) |>
  filter(dur_assess_depart > 0)


# 2. MISSING VALUES - DELETE ---------------------------------------------------

df_duration_na_rm <- df_duration |>
  filter(!is.na(acuity)) |>
  filter(!is.na(chief_comp_grp)) |>
  # TODO COULD ASSUME NA IS NO INVESTIGATION? (I.E. IMPUTATION)
  filter(!is.na(Der_EC_Investigation_All)) |>
  filter(!is.na(age))

# # BY DELETING ALL MISSING VALUES FROM KEY FIELDS LOSING 3% RECORDS
# nrow(df_duration) -nrow(df_duration_na_rm)
# 1 - df_duration_na_rm |> nrow() / df_duration |> nrow()


# 3. RE-ENGINEER VARS --------------------------------------------

## a. NEW CHIEF COMPLAINT --------------------------------------------
# AN IMPORTANT VAR AND FAR MORE INFORMATIVE THAN GROUPED COMPLAINT:

lkp_chief_comp_small <- df_duration_na_rm |>
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

df_duration_fe1 <- df_duration_na_rm |>
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

df_duration_fe2 <- df_duration_fe1 |>
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

df_duration_fe3 <- df_duration_fe2 |>
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

set.seed(1959)
df_duration_sample <- df_duration_fe3 |>
  # slice_sample(prop = 0.44)
  # slice_sample(prop = 0.30)
  # SHORTER MODEL RUN TIME FOR FASTER FEEDBACK:
  # slice_sample(prop = 0.59)
  slice_sample(n = 1e6)

gc()

# 5. OUTCOME VARIABLES ------------------------------------------

# PULL EC TYPE CODES:
vec_invst_codes_ec <- lkp_invst |>
  mutate(InvestigationCode = as.character(InvestigationCode)) |>
  mutate(InvestigationKey = str_c("invst_", InvestigationKey)) |>
  select(InvestigationKey, InvestigationCode) |>
  deframe()

# NOTE: COUNTING INSTANCES HERE TO KEEP OPTIONS OPEN.
# BUT LATER ADJUSTED TO BINARY (TEST Y/N).
list_invst_counts <- imap(vec_invst_codes_ec, function(x, y) {
  df_duration_sample %>%
    transmute({{ y }} := str_count(Der_EC_Investigation_All, str_c("^", x, "| ", x, "|,", x)))
})

df_duration_invst <- list_invst_counts |>
  reduce(bind_cols) |>
  bind_cols(df_duration_sample, y = _)

df_duration_invst <- df_duration_invst |>
  mutate(across(c(month, wkday, hour), ~ as.factor(.))) |>
  mutate(wkday = fct_relevel(wkday, "Mon")) |>
  mutate(hour = fct_relevel(hour, "12"))


# 6. FINAL PREP -----------------------------------------------------------

df_prep_duration_integer <- df_duration_invst |>
  select(
    dur_assess_depart,
    age, sex, arr_mode, acuity, chief_comp_desc, refer_sorc,
    month, wkday, hour,
    procode,
    starts_with("invst_")
  ) |>
  filter(dur_assess_depart > 0) |>
  # IF TEST, THEN AT LEAST 15 MINS:
  filter(!(if_any(starts_with("invst_"), ~ . >= 1) & dur_assess_depart < 15)) |>
  mutate(dur_assess_depart = as.integer(dur_assess_depart)) |>
  # REMOVE V. LOW FREQ TESTS:
  select(-c(invst_92, invst_93, invst_94, invst_79, invst_85, invst_63))


# 7. MODEL ----------------------------------------------------------------

mod_gam_norm_int <- mgcv::gam(
  formula = dur_assess_depart ~
    # CASE-MIX-RELATED:
    s(age, by = sex) + sex +
    arr_mode + acuity + chief_comp_desc + refer_sorc +
    # TIME-RELATED:
    month + wkday + hour +
    # PROVIDER (RANDOM INTERCEPT):
    s(procode, bs = "re") +
    # VARS OF INTEREST:
    invst_50 + invst_51 + invst_52 + invst_53 + invst_54 + invst_55 + invst_56 + invst_57 + invst_58 + invst_59 +
    invst_60 + invst_61 + invst_62 + # invst_63 +
    invst_64 + invst_65 + invst_66 + invst_67 + invst_68 + invst_69 +
    invst_70 + invst_71 + invst_72 + invst_73 + invst_74 + invst_75 + invst_76 + invst_77 + invst_78 + # invst_79 +
    invst_80 + invst_81 + invst_82 + invst_83 + invst_84 + # invst_85 +
    invst_86 + invst_87 + invst_88 + invst_89 + invst_90 + invst_91 +
    # invst_92 + invst_93 + invst_94 +
    invst_95 + invst_96 + invst_97,
  family = "gaussian",
  method = "REML",
  data = df_prep_duration_integer
)


# 8. ROBUST SEs -----------------------------------------------------------
# WITH SANDWICH PACKAGE

# THIS FORMULA COMES FROM ZELLIS PAPER PROVIDED ON SANDWICH PACKAGE WEBSITE:
# HC3
omega3 <- function(residuals, diaghat, res_dof) {
  residuals^2 / ((1 - diaghat)^2)
}

std_err_hc3 <- sqrt(
  diag(
    vcovHC(
      mod_gam_norm_int,
      omega = omega3(
        mod_gam_norm_int$residuals,
        mgcv::influence.gam(mod_gam_norm_int),
        mod_gam_norm_int$df.residual
      )
    )
  )
) |>
  enframe() |>
  filter(str_detect(name, "^invst"))


df_preplot_duration <-mod_gam_norm_int |> 
  broom::tidy(parametric = T) |> 
  filter(str_detect(term, "^invst")) |>  
  mutate(InvestigationKey = as.numeric(str_extract(term, "[:digit:]{2}"))) |>
  left_join(lkp_invst, join_by(InvestigationKey)) |>
  # REMOVE SUPERFLUOUS TEXT:
  mutate(InvestigationDescription = str_remove_all(InvestigationDescription, "[:punct:]")) |>
  mutate(InvestigationDescription = str_remove_all(InvestigationDescription, " procedure")) |> 
  relocate(InvestigationDescription, .before = term) |> 
  mutate(dura = estimate) |>
  arrange(InvestigationDescription) |>
  # select(term)
  left_join(std_err_hc3, join_by(term == name)) |> 
  mutate(lci = estimate - 1.96*value) |> 
  mutate(uci = estimate + 1.96*value) 

df_preplot_duration |> 
  saveRDS(here("data", "241204_df_preplot_duration.rds"))



