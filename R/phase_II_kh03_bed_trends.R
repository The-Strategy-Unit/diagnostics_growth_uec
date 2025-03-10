# README
# [NCDR]
# Get KH03 bed returns used in results section of Phase II report.

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


# 0. CONNECTION ----------------------------------------------------------

con_ukhf <- dbConnect(
  odbc::odbc(),
  Driver = "SQL Server",
  Server = "PRODNHSESQL101",
  Database = "NHSE_UKHF",
  Trusted_Connection = "True"
)


# 1. LOAD DATA --------------------------------------------------------

# FROM PROVIDER SAMPLE SCRIPT 
df_provider_popn <- read_rds(here("data", "df_provider_popn.rds"))

tb_kh_avl_day <- tbl(con_ukhf, in_schema("Bed_Availability", "vw_Provider_By_Sector_Available_Day_Only_Beds1"))
tb_kh_avl_nig <- tbl(con_ukhf, in_schema("Bed_Availability", "vw_Provider_By_Sector_Available_Overnight_Beds1"))
tb_kh_occ_day <- tbl(con_ukhf, in_schema("Bed_Availability", "vw_Provider_By_Sector_Occupied_Day_Only_Beds1"))
tb_kh_occ_nig <- tbl(con_ukhf, in_schema("Bed_Availability", "vw_Provider_By_Sector_Occupied_Overnight_Beds1"))

raw_beds <- 
  bind_rows(
    tb_kh_avl_day |> collect() |> mutate(metric = "avl_day"), 
    tb_kh_avl_nig |> collect() |> mutate(metric = "avl_nig"), 
    tb_kh_occ_day |> collect() |> mutate(metric = "occ_day"), 
    tb_kh_occ_nig |> collect() |> mutate(metric = "occ_nig")
  ) |> 
  clean_names()

raw_beds <- read_rds(here("data_raw", "raw_beds_kh03.rds"))

raw_beds |> 
  filter(sector == "General & Acute") |>
  filter(str_detect(organisation_code, "^R")) |>
  # FOR OUR POPULATION OF PROVIDERS:
  semi_join(df_provider_popn, join_by(organisation_code == procode)) |> 
  # count(effective_snapshot_date) |> 
  # tail()
  # count(effective_snapshot_date, data_source_file_for_this_snapshot_version) |> 
  # arrange((effective_snapshot_date)) |> 
  #
  # LOOK AT COUNT OF OUR POPN OF PROVIDERS OVER TIME:
  # count(effective_snapshot_date, organisation_code) |> 
  # count(effective_snapshot_date) |> 
  # ggplot()+
  # geom_line(aes(effective_snapshot_date, n), group = 1)+
  # geom_blank(aes(y=0))
  #
  # view("")
  # arrange(desc(effective_snapshot_date))
  count(metric, effective_snapshot_date, wt = number_of_beds) |> 
  mutate(date = as_date(effective_snapshot_date))  |> 
  arrange(desc(metric)) |> 
  group_by(dn =str_sub(metric, 5, 7), effective_snapshot_date) |> 
  mutate(p_occ = n[1]/n[2]) |> 
  ungroup() |> 
  # COUNTS:
  # ggplot()+
  # geom_line(aes(date, n, col = metric))+
  # geom_point(aes(date, n, col = metric))+
  # geom_blank(aes(y=0))+
  # facet_wrap(vars(str_sub(metric, 5, 7)), scales = "free_y")
  # PROP:
  distinct(date, dn, p_occ) |> 
  ggplot()+
  theme_bw()+
  geom_line(aes(date, p_occ))+
  geom_point(aes(date, p_occ))+
  # geom_blank(aes(y=0))+
  scale_y_continuous(labels = scales::percent, limits = c(0.5,1))+
  facet_wrap(vars(dn), scales = "free_y")

