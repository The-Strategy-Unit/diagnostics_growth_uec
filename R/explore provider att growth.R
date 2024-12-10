# 

library("here")
library("arrow")
library("dplyr")
library("purrr")
library("tidyr")
library("scales")
library("tibble")
library("forcats")
library("janitor")
library("ggplot2")
library("stringr")
library("lubridate")


# 1. LOAD ECDS DATA -------------------------------------------------------

pared_provider_sample <-
  open_dataset(here("data", "pared_provider_sample.parquet")) |>
  collect()



pared_provider_sample |> 
  group_by(fyear) |> 
  summarise(atts = n()) |> 
  pivot_wider(names_from = "fyear",
              values_from = "atts",
              names_prefix = "atts") |> 
  mutate(growth = (`atts2023/24` / `atts2019/20`) - 1)


pared_provider_sample |> 
  group_by(fyear, procode) |> 
  summarise(atts = n()) |> 
  pivot_wider(names_from = "fyear",
              values_from = "atts",
              names_prefix = "atts") |> 
  mutate(growth = (`atts2023/24` / `atts2019/20`) - 1)  |> 
  arrange(-growth)

# national growth in type 1 atts 
# from national returns 
# https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2024/11/Monthly-AE-Time-Series-October-2024.xls
# is from 
# 14,797,665 in period Apr 19 - Feb 20
# to 
# 15,062,957 in period Apr 23 - Feb 24
# i.e. +1.8%


# notes

# RRK (UNIVERSITY HOSPITALS BIRMINGHAM NHS FOUNDATION TRUST) 
#   subsumed RR1 (HEART OF ENGLAND NHS FOUNDATION TRUST) in 2018

# RNN (NORTH CUMBRIA INTEGRATED CARE NHS FOUNDATION TRUST) 
#   subsumed RNL (NORTH CUMBRIA UNIVERSITY HOSPITALS NHS TRUST) in 2019

pared_provider_sample |> 
  filter(procode != "RRK") |> 
  filter(procode != "RNN") |>
  filter(procode != "RWJ") |> 
  group_by(fyear) |> 
  summarise(atts = n()) |> 
  pivot_wider(names_from = "fyear",
              values_from = "atts",
              names_prefix = "atts") |> 
  mutate(growth = (`atts2023/24` / `atts2019/20`) - 1)

# when we remove RRK, RNN and RWJ the growth rate is similar to national 