# README
# Basis for dq_ecds_only quarto analysis (NCDR). 
# RDS objects also used in final report. 

library("DBI")
library("here") 
library("purrr") 
library("dplyr")
library("tidyr")
library("dbplyr")
library("stringr")
library("lubridate")
library("janitor")

con_sandbox_su <- dbConnect(
  odbc::odbc(),
  Driver = "SQL Server",
  Server = "",
  Database = "",
  Trusted_Connection = "True"
)

tb_ecds_only <- tbl(con_sandbox_su, in_schema("dbo", "2232_diagnostics_ecds_only"))

# 1. PULL THE TABLE -------------------------------------------------------
# EXPENSIVE BUT NECESSARY

ecds_only <- tb_ecds_only |>  
  select(-data_source) |>
  collect()

gc()

# 2. COUNT OF PROVIDERS ---------------------------------------------------

# 138 PROVIDERS TOTAL IN DATASET
# BUT ONLY 108 PROVIDERS PRESENT IN BOTH YEARS
ecds_only |> count(procode) |> count()
ecds_only |> count(fyear, procode) |> count(procode) |> filter(n == 2)


# 3. NAs BY VARIABLE ------------------------------------------------------

tmp_count <- ecds_only %>% 
  colnames() %>% 
  map(~ count(ecds_only, .data[["fyear"]], is.na(.data[[.x]]), sort = T)) %>% 
  map(list(. %>% mutate(var = (names(.))[2]))) %>% 
  map(list(. %>% rename(cat = 2))) %>% 
  map(list(. %>% mutate(is_na = as.character(cat)))) %>% 
  reduce(bind_rows) %>% 
  select(var, everything(), -cat) %>% 
  arrange(var, -n) %>% 
  group_by(var, fyear) |> 
  mutate(p = round(n/sum(n), 4)) |> 
  ungroup() |> 
  # filter(is_na == TRUE) |>
  mutate(var = str_remove_all(var, "[:punct:]" )) |>
  mutate(var = str_remove_all(var, "isnadata" )) |>
  mutate(var = snakecase::to_snake_case(var)) |> 
  relocate(n, .before = p) |> 
  arrange(var, desc(p)) 

gc()

tmp_count |>  
  saveRDS("from_ncdr_ecds_only_240917_dq_isna.RDS")

# 4. QUALITY BY PROVIDER --------------------------------------------------

dq_providers <- ecds_only |> 
  count(procode, fyear, na_inv = is.na(Der_EC_Investigation_All), na_chief = is.na(chief_comp_grp), na_acuity = is.na(acuity)) 

dq_providers |> 
  group_by(fyear, procode) |> 
  mutate(p = round(n/sum(n), 4)) |>
  ungroup() |> 
  filter(if_all(starts_with("na"), ~ . == F)) |> 
  select(procode, fyear, p) |> 
  pivot_wider(names_from = fyear, values_from = p) |> 
  arrange(-`2019/20`) |> 
  janitor::clean_names() |> 
  saveRDS("from_ncdr_ecds_only_240917_dq_provider.RDS")


# 5. EFFECT OF COVID ------------------------------------------------------

ts_daily_ecds_only <- ecds_only |> 
  count(fyear, date(dttm_arr))

ts_daily_ecds_only |> saveRDS("from_ncdr_ecds_only_240917_ts_daily.RDS")
