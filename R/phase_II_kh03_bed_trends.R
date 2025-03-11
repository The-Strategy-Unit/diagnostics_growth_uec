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
  # FOR OUR POPULATION OF 121 PROVIDERS:
  semi_join(df_provider_popn, join_by(organisation_code == procode)) |>
  mutate(date = as_date(effective_snapshot_date))  |> 
  # AND THEN JUST THOSE 108 WHO HAVE SUBMITTED ALL QUARTERS SINCE 2010:
  complete(date, organisation_code) |>
  group_by(organisation_code) |>
  filter(!is.na(sum(number_of_beds))) |>
  ungroup() |>
  # distinct(organisation_code)
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
  count(metric, date, wt = number_of_beds) |> 
  arrange(desc(metric)) |> 
  group_by(dn =str_sub(metric, 5, 7), date) |> 
  mutate(p_occ = n[1]) |> 
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
  # scale_y_continuous(labels = scales::percent, limits = c(0.5,1))+
  facet_wrap(vars(dn), scales = "free_y")

# TODO
# OVERALL TREND IN NUMBERS
# RATES 


# OVERNIGHT BEDS OCCUPIED IN 2019/20 AND 2023/24 ----------------

raw_beds |> 
  filter(sector == "General & Acute") |>
  filter(str_detect(organisation_code, "^R")) |>
  # FOR OUR POPULATION OF 121 PROVIDERS:
  semi_join(df_provider_popn, join_by(organisation_code == procode)) |>
  mutate(date = as_date(effective_snapshot_date))  |> 
  # AND THEN JUST THOSE 108 WHO HAVE SUBMITTED ALL QUARTERS SINCE 2010:
  complete(date, organisation_code) |>
  group_by(organisation_code) |>
  filter(!is.na(sum(number_of_beds))) |>
  ungroup() |>
  #
  count(metric, date, wt = number_of_beds) |> 
  filter(metric == "occ_nig") |> 
  filter(
    between(
      date, 
      as_date("2019-04-01"), 
      # EXCLUDE COVID:
      # as_date("2020-03-31")
      as_date("2019-12-31")
      ) 
    |
      between(
        date, 
        as_date("2023-04-01"), 
      # EXCLUDE MARCH TO MIRROR ABOVE:
        # as_date("2024-03-31")
        as_date("2023-12-31")
      ) 
    ) |> 
  # ggplot()+
  # theme_bw()+
  # # geom_line(aes(date, n))+
  # geom_point(aes(date, n))+
  # # geom_blank(aes(y=0))+
  # # scale_y_continuous(labels = scales::percent, limits = c(0.5,1))+
  # # facet_wrap(vars(dn), scales = "free_y")
  # NULL
  group_by(year_baseline = year(date) < 2021) |> 
  mutate(mean_fyear = mean(n)) |> 
  ungroup() |> 
  distinct(year_baseline, mean_fyear) |> 
  mutate(p_growth = mean_fyear[2]/mean_fyear[1])

# TODO:
# 10% more beds OCCUPIED
# Given our model results, that means x more likely than in baseline year
# Baseline number of tests done at these providers.
# Final year number of tests done at these providers.
# What proportion due to this effect?


# OVERNIGHT BEDS P OCCUPIED 2019/20 AND 2023/24 ----------------

raw_beds |> 
  filter(sector == "General & Acute") |>
  filter(str_detect(organisation_code, "^R")) |>
  # FOR OUR POPULATION OF 121 PROVIDERS:
  semi_join(df_provider_popn, join_by(organisation_code == procode)) |>
  mutate(date = as_date(effective_snapshot_date))  |> 
  # AND THEN JUST THOSE 108 WHO HAVE SUBMITTED ALL QUARTERS SINCE 2010:
  complete(date, organisation_code) |>
  group_by(organisation_code) |>
  filter(!is.na(sum(number_of_beds))) |>
  ungroup() |>
  #
  count(metric, date, wt = number_of_beds) |> 
  filter(metric %in% c("occ_nig", "avl_nig")) |>
  filter(
    between(
      date, 
      as_date("2019-04-01"), 
      # as_date("2020-03-31")
      as_date("2019-12-31")
    ) 
    |
      between(
        date, 
        as_date("2023-04-01"), 
        # as_date("2024-03-31")
        as_date("2023-12-31")
      ) 
  ) |> 
  arrange((metric)) |>
  # ggplot()+
  # theme_bw()+
  # # geom_line(aes(date, n))+
  # geom_point(aes(date, n, col = metric))+
  # # geom_blank(aes(y=0))+
  # # scale_y_continuous(labels = scales::percent, limits = c(0.5,1))+
  # # facet_wrap(vars(dn), scales = "free_y")
  # NULL
  group_by(metric, year_baseline = year(date) < 2021) |> 
  mutate(mean_fyear = mean(n)) |> 
  ungroup() |> 
  distinct(metric, year_baseline, mean_fyear) |> 
  group_by(year_baseline) |> 
  mutate(p_growth = mean_fyear[2]/mean_fyear[1]) |> 
  ungroup() |> 
  arrange(desc(year_baseline))

# THERE WERE 90.8% BEDS OCC
# NOW 91% BEDS OCC

