# README
# Wrangling to create Table 1 from analysis data set.

library("gt")
library("DBI")
library("here")
library("arrow")
library("dbplyr")
library("dplyr")
library("tidyr")
library("readr")
library("forcats")
library("ggplot2")
library("janitor")
library("stringr")
library("gtExtras")
library("lubridate")


# 0. CONNECTIONS ----------------------------------------------------------

con_sandbox_su <- dbConnect(
  odbc::odbc(),
  Driver = "SQL Server",
  Server = "PRODNHSESQL101",
  Database = "NHSE_Sandbox_StrategyUnit",
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
  select(-matches("fyear|disdest|dur|inj|treat_|imd_|ethnic|_icb")) |> 
  mutate(age = as.integer(age)) |> 
  mutate(age_grp = case_when(
    age %in% 0:9 ~ "00-09",
    age %in% 10:19 ~ "10-19",
    age %in% 20:29 ~ "20-29",
    age %in% 30:39 ~ "30-39",
    age %in% 40:49 ~ "40-49",
    age %in% 50:59 ~ "50-59",
    age %in% 60:69 ~ "60-69",
    age %in% 70:79 ~ "70-79",
    age %in% 80:89 ~ "80-89",
    age >= 90 ~ "90+",
    T ~ "Not known"
  )) 

ecds_extract |>   
  # group_by(fyear) |>
  summarise(n = n()) |>
  mutate(grouping = "total") |>
  mutate(subgroup = "-") |>
  bind_rows(
    ecds_extract |>
      group_by(age_grp) |>
      summarise(n = n()) |>
      mutate(grouping = "age group") |>
      rename(subgroup = age_grp)
  ) |>
  bind_rows(
    ecds_extract |>
      group_by(sex) |>
      summarise(n = n()) |>
      mutate(sex = ifelse(sex == "f", "Female", "Male")) |>
      mutate(grouping = "sex") |>
      rename(subgroup = sex)
  ) |>
  bind_rows(
    ecds_extract |>
      mutate(chief_comp_grp = ifelse(as.character(chief_comp_grp) == "Not applicable to child terms",
                                     NA_character_,
                                     as.character(chief_comp_grp)
      )) |>
      group_by(chief_comp_grp) |>
      summarise(n = n()) |>
      mutate(grouping = "chief complaint") |>
      rename(subgroup = chief_comp_grp)
  ) |>
  bind_rows(
    ecds_extract |>
      mutate(acuity_desc = substr(
        as.character(acuity_desc),
        1,
        nchar(as.character(acuity_desc)) - 38
      )) |>
      mutate(acuity_desc = factor(acuity_desc,
                                  levels = c(
                                    "Non-urgent",
                                    "Standard",
                                    "Urgent",
                                    "Very urgent",
                                    "Immediate resuscitation"
                                  )
      )) |>
      group_by(acuity_desc) |>
      summarise(n = n()) |>
      mutate(grouping = "acuity") |>
      rename(subgroup = acuity_desc)
  ) |>
  select(grouping, subgroup, attendances = n) |>
  group_by(grouping) |>
  mutate(p_atts = attendances / sum(attendances)) |> 
  select(
    grouping,
    subgroup,
    `attendances 2023/24` = attendances,
    `p atts 2023/24` = p_atts
  ) |>
  mutate(subgroup = ifelse(subgroup == "NA" |
                             is.na(subgroup), "Not known", subgroup)) |>
  ungroup() |> 
  saveRDS("df_phase_II_prep_table_1.rds")
