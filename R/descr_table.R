# 0 set-up ----
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
library("scales")
library("gt")


# 1 load ecds data ----

# source("create_study_dataset.R")

pared_provider_sample <- 
  open_dataset(here("data", "pared_provider_sample.parquet")) |> 
  collect()









pared_provider_sample |> 
  group_by(fyear) |> 
  summarise(n = n()) |> 
  mutate(grouping = "total") |> 
  mutate(subgroup = "-") |> 
  bind_rows(pared_provider_sample |> 
              group_by(fyear, age_grp) |> 
              summarise(n = n()) |> 
              mutate(grouping = "age group") |> 
              rename(subgroup = age_grp)) |> 
  bind_rows(pared_provider_sample |> 
              group_by(fyear, sex) |> 
              summarise(n = n()) |> 
              mutate(sex = ifelse(sex == "f", "female", "male")) |> 
              mutate(grouping = "sex") |> 
              rename(subgroup = sex)) |> 
  bind_rows(pared_provider_sample |> 
              mutate(chief_comp_grp = ifelse(as.character(chief_comp_grp) == "Not applicable to child terms",
                                             NA_character_,
                                             as.character(chief_comp_grp))) |> 
              group_by(fyear, chief_comp_grp) |>
              summarise(n = n()) |> 
              mutate(grouping = "chief complaint") |> 
              rename(subgroup = chief_comp_grp)) |>  
  bind_rows(pared_provider_sample |> 
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
                           | is.na(subgroup) , "not known", subgroup)) |> 
  gt(groupname_col = "grouping") |> 
  tab_options(row_group.as_column = TRUE) |> 
  fmt_number(columns = c(`attendances 2019/20`, `attendances 2023/24`),
               decimals = 0) |> 
  fmt_percent(columns = c(`p_atts_2019/20`, `p_atts_2023/24`),
             decimals = 1) |> 
  cols_merge(columns = c(`attendances 2019/20`, `p_atts_2019/20`),
             pattern = "{1} ({2})") |> 
  cols_merge(columns = c(`attendances 2023/24`, `p_atts_2023/24`),
             pattern = "{1} ({2})") |> 
  cols_label(subgroup = "", 
             `attendances 2019/20` = '2019/20',
             `attendances 2023/24` = '2023/24') |> 
  tab_spanner(label = 'attendances (%)',
              columns = c(`attendances 2019/20`, `attendances 2023/24`))










