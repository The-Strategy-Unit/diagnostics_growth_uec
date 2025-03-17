# README
# [NCDR]
# Prepare data for models used in Phase II.

library("DBI")
library("here") 
library("dplyr")
library("purrr") 
library("furrr") 
library("readr") 
library("tidyr")
library("dbplyr")
library("tibble") 
library("forcats")
library("ggplot2") 
library("janitor")
library("stringr")
library("lubridate")


# 0. CONNECTIONS ----------------------------------------------------------

con_sandbox_su <- dbConnect(
  odbc::odbc(),
  Driver = "SQL Server",
  Server = "PRODNHSESQL101",
  Database = "NHSE_Sandbox_StrategyUnit",
  Trusted_Connection = "True"
)

con_sus_plus <- dbConnect(
  odbc::odbc(),
  Driver = "SQL Server",
  Server = "PRODNHSESQL101",
  Database = "NHSE_SUSPlus_Live",
  Trusted_Connection = "True"
)

con_nhse_reference <- dbConnect(
  odbc::odbc(),
  Driver = "SQL Server",
  Server = "PRODNHSESQL101",
  Database = "NHSE_Reference",
  Trusted_Connection = "True"
)

# 1. LOAD DATA --------------------------------------------------------

tb_ref_invst_ec <- tbl(con_nhse_reference, in_schema("dbo", "tbl_Ref_DataDic_ECDS_Investigation"))

lkp_invst <- tb_ref_invst_ec |>
  select(InvestigationKey, InvestigationCode, InvestigationDescription) |>
  # # REMOVE NO INVESTIGATION FROM SEARCH LIST
  # (NOTE: ITS PRESENCE DOESN'T INDICATE NO INVESTIGATION):
  filter(InvestigationKey != 98) |>
  collect() |> 
  mutate(InvestigationCode = as.character(InvestigationCode))

vec_provider_selection <- c(
  # FROM DATA QUALITY ASSESSMENT:
  "RKB", "RCB", "RHM", "RCU", "RVJ",
  "RWW", "RK9", "RWH", "RLT", "RFF", 
  "RVW", "REM", "RBK", "RTF", "RX1", 
  "RYR", "RQW", "RWD", "RYJ", "RNN", 
  "RNZ", "RTG", "RHQ", "RXC", "RFS"
) |> 
  enframe(name = NULL, value = "procode") |>
  pull()

# SHARES FOUNDATIONAL QUERY WITH INDUSTRIAL ACTION WORK (FILTER FOR DESIRED YEAR):
tb_ecds_0 <- tbl(con_sandbox_su, in_schema("dbo", "1208_ind_action_model_extract_ecds_0"))


ecds_extract <- tb_ecds_0 |>
  filter(fyear == "2023/24") |>
  filter(procode %in% local(vec_provider_selection)) |>
  collect() 

ecds_extract <- ecds_extract |> 
  select(-matches("fyear|disdest|dur|inj|treat_|imd_|ethnic|_icb")) 


# 2. MISSING VALUES - DELETE RECORDS -----------------------------------------------

df_odds_na_rm <- ecds_extract |>
  filter(!is.na(acuity)) |>
  # CHIEF COMPLAINT MISSINGS HAVE BIGGEST IMPACT.
  filter(!is.na(chief_comp_grp)) |>
  # ALTERNATIVELY, COULD ASSUME NA IS NO INVESTIGATION? (AND IMPUTE)
  filter(!is.na(invst_all)) |>
  filter(!is.na(age)) 


# # BY DELETING ALL MISSING VALUES FROM KEY FIELDS,
# WE ARE LOSING 135,744 RECORDS (5.5%).
# nrow(ecds_extract) -nrow(df_odds_na_rm)
# 1 - df_odds_na_rm |> nrow() / ecds_extract |> nrow()


# 3. RE-ENGINEER VARS --------------------------------------------

## a. NEW CHIEF COMPLAINT --------------------------------------------
# AN IMPORTANT VAR AND FAR MORE INFORMATIVE THAN GROUPED COMPLAINT:

lkp_chief_comp_small <- df_odds_na_rm |>
  count(chief_comp_desc, chief_comp_grp, sort = T) |>
  mutate(p = round(n / sum(n), 4) * 100) |>
  # ungroup() |>
  # SMALL NUMBERS (LOW % OF CASES)
  filter(p < 0.1) |>
  # TOTAL % OF ALL CASES:
  group_by(chief_comp_grp, chief_comp_desc) |>
  summarise(n = sum(n), p = sum(p)) |> 
  ungroup() |> 
  arrange(n) |> 
  # head(20)
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
  # count(chief_comp_desc_small, wt = n, sort = T)
  select(-c(chief_comp_grp, n, p))

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
  mutate(day = day(dttm_arr)) |> 
  # select(-dttm_arr) |> 
  identity()


# 4. ADD OCCUPANCY VARIABLE --------------------------------------------------------

lkp_occupancy <- readRDS("lkp_occupancy.RDS")

df_odds_occ <- df_odds_fe3 |> 
  left_join(
    lkp_occupancy, 
    join_by(procode, month, day, hour)
  )


# 4.* SENSITIVITY ANALYSIS ------------------------------------------------

# # *ONLY RUN WHEN SENSITIVITY ANALYSIS REQUIRED*
# df_odds_fe3 <- df_odds_fe3 |> 
#   # head(20) |> 
#   # select(starts_with("dttm")) |> 
#   mutate(half_stay = 
#            round_half_up(
#            as.integer(difftime(dttm_depart, dttm_arr, units = "mins"))/2
#            )
#          ) |> 
#   mutate(dttm_arr_half = dttm_arr + minutes(half_stay)) |> 
#   mutate(month_half = month(dttm_arr_half)) |>
#   mutate(day_half = day(dttm_arr_half)) |> 
#   mutate(hour_half = hour(dttm_arr_half))
#   
# lkp_occupancy <- readRDS("lkp_occupancy.RDS")
# 
# df_odds_occ <- df_odds_fe3 |> 
#   left_join(
#     lkp_occupancy, 
#     join_by(procode, month_half == month, day_half == day, hour_half == hour)
#   ) 
#   
# 5. SAMPLE 1 MILLION RECORDS (~43%) ------------------------

set.seed(1822)
df_odds_sample <- df_odds_occ |>
  slice_sample(n = 1e6)

gc()


# # SHORTER MODEL RUN TIME FOR FASTER FEEDBACK
# set.seed(1822)
# df_odds_sample <- df_odds_occ |>
#   slice_sample(prop = 0.1)
# 
# gc()

# 6. OUTCOME VARIABLES ------------------------------------------

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
    transmute({{ y }} := str_count(invst_all, str_c("^", x, "| ", x, "|,", x)))
})

df_odds_invst <- list_invst_counts |>
  reduce(bind_cols) |>
  bind_cols(df_odds_sample, y = _)

gc()

# 7. FINAL DATA PREP ------------------------------------------------------

df_prep_binary <- df_odds_invst |>
  ### REMOVE VARS THAT WON'T BE USED IN BASIC MODEL:
  select(-matches("acuity_desc|^dttm|grp|invst_all|day|month|hour")) |>
  # colnames()
  # SWITCH OUTCOMES TO BINARY:
  mutate(across(starts_with("invst_"), ~ if_else(. > 0, 1, 0))) |> 
  mutate(occ_decile = fct_relevel(as.factor(occ_decile), "1")) |> 
  mutate(across(c(sex, arr_mode, acuity, procode), ~ as.factor(.))) |> 
  mutate(sex = fct_relevel(sex, "m")) |> 
  mutate(acuity = fct_relevel(acuity, "1")) |> 
  mutate(arr_mode = fct_relevel(arr_mode, "walk_in")) |>
  mutate(age = as.integer(age)) 

gc()
