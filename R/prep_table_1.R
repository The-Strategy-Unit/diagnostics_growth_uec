# README
# Wrangling to create Table 1 from provider sample. 

library("gt")
library("here")
library("arrow")
library("dplyr")
library("tidyr")
library("readr")
library("forcats")
library("ggplot2")
library("janitor")
library("stringr")
library("gtExtras")
library("lubridate")

provider_sample <- 
  open_dataset(here("data_raw", "provider_sample.parquet")) |> 
  collect()

provider_sample |> 
  group_by(fyear) |> 
  summarise(n = n()) |> 
  mutate(grouping = "total") |> 
  mutate(subgroup = "-") |> 
  bind_rows(provider_sample |> 
              group_by(fyear, age_grp) |> 
              summarise(n = n()) |> 
              mutate(grouping = "age group") |> 
              rename(subgroup = age_grp)) |> 
  bind_rows(provider_sample |> 
              group_by(fyear, sex) |> 
              summarise(n = n()) |> 
              mutate(sex = ifelse(sex == "f", "Female", "Male")) |> 
              mutate(grouping = "sex") |> 
              rename(subgroup = sex)) |> 
  bind_rows(provider_sample |> 
              mutate(chief_comp_grp = ifelse(as.character(chief_comp_grp) == "Not applicable to child terms",
                                             NA_character_,
                                             as.character(chief_comp_grp))) |> 
              group_by(fyear, chief_comp_grp) |>
              summarise(n = n()) |> 
              mutate(grouping = "chief complaint") |> 
              rename(subgroup = chief_comp_grp)) |>  
  bind_rows(provider_sample |> 
              mutate(acuity_desc = substr(as.character(acuity_desc), 
                                          1, 
                                          nchar(as.character(acuity_desc)) - 38)) |> 
              mutate(acuity_desc = factor(acuity_desc, 
                                          levels = c("Non-urgent", 
                                                     "Standard", 
                                                     "Urgent", 
                                                     "Very urgent", 
                                                     "Immediate resuscitation"))) |>
              
              group_by(fyear, acuity_desc) |> 
              summarise(n = n()) |> 
              mutate(grouping = "acuity") |> 
              rename(subgroup = acuity_desc)) |> 
  select(fyear, grouping, subgroup, attendances = n) |> 
  group_by(fyear, grouping) |> 
  mutate(p_atts =  attendances / sum(attendances)) |> 
  pivot_wider(names_from = "fyear", 
              values_from = c("attendances", "p_atts"),
              values_fill = 0) |> 
  select(grouping, subgroup, 
         `attendances 2019/20` = `attendances_2019/20`, 
         `p_atts_2019/20`, 
         `attendances 2023/24` = `attendances_2023/24`, 
         `p_atts_2023/24`) |> 
  mutate(subgroup = ifelse(subgroup == "NA" 
                           | is.na(subgroup) , "Not known", subgroup)) |> 
  ungroup() |> 
  saveRDS(here("data", "prep_table_1.rds"))
