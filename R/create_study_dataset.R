# README
# Pulls the provider sample from ecds_only table, removes march, 
# and fixes data types. The resulting study data frame should serve
# as the basis for all modelling work in the project. (NCDR)

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


# 0. SETUP ----------------------------------------------------------------

## a. connections --------------------------------------------------------

con_sandbox_su <- dbConnect(
  odbc::odbc(),
  Driver = "SQL Server",
  Server = "PRODNHSESQL101",
  Database = "NHSE_Sandbox_StrategyUnit",
  Trusted_Connection = "True"
)

tb_ecds_only <- tbl(con_sandbox_su, in_schema("dbo", "2232_diagnostics_ecds_only"))

## b. pull ec only table -------------------------------------------------------

raw_provider_sample <- tb_ecds_only |>
  # BASED ON PROVIDER SAMPLE OF 16 IDENTIFIED IN DQ_ECDS_ONLY QUARTO DOC:
  filter(procode %in% c(
    "RKB",
    "RHM",
    "RVW",
    "RWJ",
    "RCB",
    "RRK",
    "RYR",
    "RWW",
    "RCU",
    "RX1",
    "RJR",
    "RXF",
    "RYJ",
    "RWD",
    "RK9",
    "RNN"
  )) |>
  select(-data_source) |>
  collect()

gc()
gc()

# 1. MASTER DF ------------------------------------------------------------
# FOR ALL MODELLING EXERCISES

## a. data types ---------------------------------------------------------

provider_sample <- raw_provider_sample |> 
  mutate(inj_flag = as.factor(as.numeric(as.logical(inj_flag)))) |> 
  mutate(age = as.integer(age)) |> 
  mutate(across(
    matches("fyear|att_|arr_mode|^disd|^acu|^chief|^inj|^sex|_grp|^imd|procode|lacd|region|lsoa"),
    ~ as.factor(.)
  )) |>
  mutate(acuity = as.ordered(acuity)) 

# TO SUMMARISE IN SKIM QUARTO:
# provider_sample |>
#   skimr::skim() |> 
#   saveRDS("from_ncdr_ecds_only_240924_skim.rds")
