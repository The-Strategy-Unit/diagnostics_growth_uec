# README
# Basis for dq trends quarto analysis (NCDR)
# RDS objects also used in final report. 

library("DBI")
library("here") 
library("purrr") 
library("dplyr")
library("tidyr")
library("dbplyr")
library("ggplot2")
library("stringr")
library("janitor")
library("lubridate")

con_sandbox_su <- dbConnect(
  odbc::odbc(),
  Driver = "SQL Server",
  Server = "",
  Database = "",
  Trusted_Connection = "True"
)

tb_trend <- tbl(con_sandbox_su, in_schema("dbo", "2232_diagnostics_trend"))

# 1. TIMESERIES OF PROVIDER ATTENDANCES ------------

tb_trend |> 
  count(fyear, procode, wt = n) |> 
  collect() |>
  saveRDS("from_ncdr_240917_ts_prov_att.rds")

# 2. CODING OF MULITPLE TESTS ----------------------
# where multiple investigations have been carried out, multiple codes 
# should be recorded. We can tell this by ruling out providers who never
# record multiple tests of any type.

tmp_multicode <- tb_trend |> 
  count(fyear, procode, Der_Investigation_All, wt = n) |> 
  collect() |> 
  rename(invest = Der_Investigation_All) 

gc()

tmp_sensitivity <- tibble(
  invest_code = c(
    "01", # = X-ray plain film
    "03", # = Haematology
    "05", # = Biochemistry
    "10", # = Ultrasound
    "11", # = Magnetic resonance imaging
    "12" # = Computerised tomography
  )
) |>
  mutate(data = pmap(
    list(invest_code),
    \(x)
    tmp_multicode |>
      mutate(code_occurs = str_count(invest, x)) |>
      # (AS % OF TESTS ISSUED BETWEEN 1 AND 4 TIMES)
      filter(code_occurs >= 1 & code_occurs <= 4) |>
      count(fyear, procode, code_occurs, wt = n) |>
      group_by(fyear, procode) |>
      mutate(p = round(n / sum(n), 3)) |>
      ungroup()
  ))

gc()

tmp_sensitivity |> saveRDS("from_ncdr_trends_240917_dq_multicoding.rds")


# 3. AEA-STYLE TEST CODING: TIMESERIES OF QUALITY BY PROVIDER----------------------------------------------------

# THIS WILL TELL US HOW MANY ARE ACTUALLY NA - NOT CODED 24, 99
provider_invest_coding <- tb_trend |> 
  count(fyear, procode,
        is.na(Der_Investigation_All),
        Der_Investigation_All == "24",
        Der_Investigation_All == "99",
        wt = n) |> 
  collect() |>
  group_by(procode, fyear) |>
  mutate(p = n/sum(n)) |>
  ungroup() |>
  janitor::clean_names()

provider_invest_coding_summary <- provider_invest_coding |> 
  arrange(procode, fyear) |> 
  rename(
    NA_invest_code = is_na_der_investigation_all,
    code_24 = der_investigation_all_24,
    code_99 = der_investigation_all_99,
  ) |> 
  filter(NA_invest_code == F) |>
  group_by(fyear, procode) |> 
  mutate(p_na_invest = 1 - sum(p)) |>
  ungroup() |> 
  # print(n=36)
  identity()
# FOR MANY PROVIDERS THERE IS A SWITCH FROM CODING 24 TO CODING NA
# GRAPH THESE PROVIDERS
provider_invest_coding_summary <- provider_invest_coding_summary |> 
  mutate(p_code_24 = if_else(code_24 == T, p, NA_real_)) |> 
  mutate(p_code_99 = if_else(code_99 == T, p, NA_real_)) |> 
  mutate(p_valid = if_else(NA_invest_code == F & code_24 == F & code_99 == F, p, NA_real_)) |> 
  select(-p) |> 
  group_by(fyear, procode) |> 
  summarise(across(starts_with("p_"), ~ mean(., na.rm = T))) |> 
  ungroup() |> 
  pivot_longer(cols = starts_with("p_"), names_to = "code", values_to = "p") |> 
  mutate(code = str_remove_all(code, "p_|_invest")) |> 
  mutate(p = if_else(is.nan(p), 0, p)) 

provider_invest_coding_summary |> 
  saveRDS("from_ncdr_trends_240917_dq_prov_na_24_99.rds")