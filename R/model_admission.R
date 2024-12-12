# README
# Build model for case-mix adjusted odds of admission, having
# received a seecified test. (NCDR)

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


# 0. RAW LOAD ----------------------------------------------------------------

provider_sample <- 
  open_dataset(here("provider_sample.parquet")) |> 
  collect() 

gc()

lkp_invst <- readRDS(here("lkp_invst.rds"))

# 1. ADMISSION DF ----------------------------------------------------------

# WHAT INFO DO WE NOT NEED / NOT HAVE AT THE TIME OF ASSESSMENT? 
df_admission <- provider_sample |>
  # REMOVE MARCH FROM BOTH YEARS TO MINIMISE COVID BIAS:
  filter(is_march == 0) |> 
  select(-matches("age_grp|lso|^reg|^lacd|is_march|^n_|^flag_odd"), n_diag_ec) |> 
  mutate(is_adm = if_else(disdest_grp == "admitted", 1, 0))

# 2. MISSING VALUES - DELETE ---------------------------------------------------

df_admission_na_rm <- df_admission |>
  filter(!is.na(acuity)) |>
  filter(!is.na(chief_comp_grp)) |>
  # TODO COULD ASSUME NA IS NO INVESTIGATION? (I.E. IMPUTATION)
  filter(!is.na(Der_EC_Investigation_All)) |>
  filter(!is.na(age)) 


# 3. RE-ENGINEER VARS --------------------------------------------

## a. NEW CHIEF COMPLAINT --------------------------------------------
# AN IMPORTANT VAR AND FAR MORE INFORMATIVE THAN GROUPED COMPLAINT:

lkp_chief_comp_small <- df_admission_na_rm |>
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

df_admission_fe1 <- df_admission_na_rm |> 
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

df_admission_fe2 <- df_admission_fe1 |> 
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

df_admission_fe3 <- df_admission_fe2 |>
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

# 4. SAMPLE X% --------------------------------------------------------

set.seed(1959)
df_admission_sample <- df_admission_fe3 |>
  # slice_sample(prop = 0.44)
  # slice_sample(prop = 0.30)
  # SHORTER MODEL RUN TIME FOR FASTER FEEDBACK:
  slice_sample(prop = 0.10)

gc()

# 5. INVESTIGATION VARIABLES ------------------------------------------

# PULL EC TYPE CODES:
vec_invst_codes_ec <- lkp_invst |> 
  mutate(InvestigationCode = as.character(InvestigationCode)) |>
  mutate(InvestigationKey = str_c("invst_", InvestigationKey)) |>
  select(InvestigationKey, InvestigationCode) |>
  deframe()

# NOTE: COUNTING INSTANCES HERE TO KEEP OPTIONS OPEN.
# BUT LATER ADJUSTED TO BINARY (TEST Y/N).
list_invst_counts <- imap(vec_invst_codes_ec, function(x, y) {
  df_admission_sample %>%
    transmute({{ y }} := str_count(Der_EC_Investigation_All, str_c("^", x, "| ", x, "|,", x)))
})

df_admission_invst <- list_invst_counts |>
  reduce(bind_cols) |>
  bind_cols(df_admission_sample, y = _)

# 6.MODEL ---------------------------------------------------------------

df_admission_invst <- df_admission_invst |> 
  mutate(across(c(month, wkday, hour), ~ as.factor(.))) |> 
  mutate(wkday = fct_relevel(wkday, "Mon")) |> 
  mutate(hour = fct_relevel(hour, "12")) 

df_prep_admission <- df_admission_invst |> 
  # SWITCH OUTCOMES TO BINARY:
  mutate(across(starts_with("invst_"), ~ if_else(. > 0, 1, 0))) |> 
  select(
    is_adm,
    fyear,
    age, sex, arr_mode, acuity, chief_comp_desc, refer_sorc,
    month, wkday, hour,
    procode, 
    starts_with("invst_")
  ) |> 
  mutate(across(starts_with("invst_"), ~ as.factor(.))) 

# df_prep_admission |> 
#   count(invst_92)
# select(starts_with("invst")) |> 
#   str()
# 
# glimpse(df_prep_admission)


# TODO NEAR-ZERO VARIANCE. DEPENDING ON SAMPLE SIZE THERE MAY BE OTHERS.
df_prep_admission <- df_prep_admission |> 
  select(-c(invst_92, invst_93))

mod_admission <- mgcv::bam(
  formula = is_adm ~
    # DEMOGRAPHICS:
    s(age, by = sex) + sex + # imd_dec +
    # CASE-MIX-RELATED:
    arr_mode + acuity + chief_comp_desc + refer_sorc + # DIAGNOSIS 01 desc # N_dIAG
    arr_mode*chief_comp_desc +
    # TIME-RELATED:
    # is_winter + is_wkend + is_night +
    fyear + month + wkday + hour + 
    # month + s(hour, by = wkday) + wkday +
    # PROVIDER (RANDOM INTERCEPT):
    s(procode, bs = "re") +
    # INVESTIGATION ALONE:
    invst_50 + invst_51 + invst_52 + invst_53 + invst_54 + invst_55 + invst_56 +
    invst_57 + invst_58 + invst_59 + invst_60 + invst_61 + invst_62 + invst_63 + 
    invst_64 + invst_65 + invst_66 + invst_67 + invst_68 + invst_69 + invst_70 + 
    invst_71 + invst_72 + invst_73 + invst_74 + invst_75 + invst_76 + invst_77 +
    invst_78 + invst_79 + invst_80 + invst_81 + invst_82 + invst_83 + invst_84 +
    invst_85 + invst_86 + invst_87 + invst_88 + invst_89 + invst_90 + invst_91 +
    # invst_92 + invst_93 +
    invst_94 + invst_95 + invst_96 + invst_97 +
    # VARS OF INTEREST:
    invst_50*fyear +
    invst_51*fyear +
    invst_52*fyear +
    invst_53*fyear +
    invst_54*fyear +
    invst_55*fyear +
    invst_56*fyear +
    invst_57*fyear +
    invst_58*fyear +
    invst_59*fyear +
    invst_60*fyear +
    invst_61*fyear +
    invst_62*fyear +
    invst_63*fyear +
    invst_64*fyear +
    invst_65*fyear +
    invst_66*fyear +
    invst_67*fyear +
    invst_68*fyear +
    invst_69*fyear +
    invst_70*fyear +
    invst_71*fyear +
    invst_72*fyear +
    invst_73*fyear +
    invst_74*fyear +
    invst_75*fyear +
    invst_76*fyear +
    invst_77*fyear +
    invst_78*fyear +
    invst_79*fyear +
    invst_80*fyear +
    invst_81*fyear +
    invst_82*fyear +
    invst_83*fyear +
    invst_84*fyear +
    invst_85*fyear +
    invst_86*fyear +
    invst_87*fyear +
    invst_88*fyear +
    invst_89*fyear +
    invst_90*fyear +
    invst_91*fyear +
    # invst_92 +
    # invst_93 +
    invst_94*fyear +
    invst_95*fyear +
    invst_96*fyear +
    invst_97*fyear,
  family = "binomial",
  method = "REML",
  data = df_prep_admission
)

mod_admission |>
  # broom::tidy(parametric = F) |>
  broom::tidy(parametric = TRUE) |>
  mutate(odds = exp(estimate), .before = estimate) |> 
  saveRDS("from_ncdr_241106_admission_odds.rds")

