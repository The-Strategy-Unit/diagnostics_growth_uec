# README
# This file sets out a method to attribute the total growth in attendances
# with a test (dT) into growth due to:
# [dA] overall attendances (previously dD)
# [dC] changes in Casemix
# [dP] changes in Practice

# MULTIPLICATIVE MODEL:
# 1+T = (1+D)(1+C)(1+P)

# 0. SET UP ---------------------------------------------------------------

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

# 1. LOAD DATA AND CREATE ODDS MODEL --------------------------------------------

# PROBABLY BEST TO RUN SCRIPT IN TWO BATCHES.

# SO BATCH 1:
tictoc::tic()
df3 <- bind_rows(
  readRDS("models_odds_top_1to4.rds"),
  readRDS("models_odds_top_5to8.rds"),
  readRDS("models_odds_top_9to12.rds"),
  readRDS("models_odds_top_13to16.rds")
) 
tictoc::toc()
# ~90 SECONDS PER GB
gc()
# AND NOW RUN THE REST OF THE SCRIPT BEFORE RETURNING TO BATCH 2. 

# SECOND BATCH:
# df3 <- bind_rows(
#   readRDS("models_odds_top_17to20.rds"),
#   readRDS("models_odds_top_21to28.rds"),
#   readRDS("models_odds_top_29to32.rds")
# ) 
# tictoc::toc()
# # ~90 SECONDS PER GB
# gc()


# 2. [dT] ESTABLISH ABSOLUTE GROWTH IN ATTENDANCES WITH (1 OR MORE) TEST ------

df3 <- df3 |>
  mutate(df_growth = map(data, function(df) {
    df |>
      filter(invst == 1) |>
      group_by(fyear) |>
      summarise(tests = n()) |>
      pivot_wider(
        names_from = "fyear",
        values_from = "tests",
        names_prefix = "tests_"
      ) |>
      clean_names() |>
      mutate(dT = tests_2023_24/tests_2019_20 - 1)
  })) 

# df3$df_growth[[1]]

# 3. [pdA] ESTABLISH RELATIVE GROWTH IN ATTENDANCES --------------------------

df3 <- df3 |> 
  mutate(pdA = map_dbl(data, function(df){
    df |> 
      group_by(fyear) |>
      summarise(atts = n()) |>
      pivot_wider(names_from = "fyear", values_from = "atts", names_prefix = "atts_") |>
      clean_names() |> 
      mutate(atts_growth_rel = atts_2023_24 / atts_2019_20) |>
      pull(atts_growth_rel) 
    
  }), .after = model)


# 4. [dA] USE ABOVE TO DERIVE ACTIVITY GROWTH  -------------------------

df4 <- df3 |> 
  mutate(df_growth = map2(df_growth, pdA, function(df, x){
    df |> 
      # mutate(dD = (x-1)*tests_2019_20)
      mutate(dA = (x-1))
  }))

# df4$df_growth[[2]]

# 5. [dP] -------------------------------------------------------------

## a. PREDICT TESTS FOR Y1. THEN FOR YR2 GIVEN THE CASEMIX OF YR1------
df5 <- df4 |> 
  mutate(df_pred = pmap(list(data, model), function(df, m){
    df |> 
      filter(fyear == "2019/20") %>%
      mutate(pred_1920 = predict(m, newdata = ., type = "response")) |>
      # THEN, IF THE EXACT SAME CASEMIX AS SEEN IN 2019/20 ARRIVED IN 2023/24 WHAT WOULD BE PREDICTED:
      mutate(fyear = factor("2023/24", levels = c("2019/20", "2023/24"))) %>%
      mutate(pred_2324 = predict(m, newdata = ., type = "response")) |>
      select(starts_with("pred_")) |> 
      summarise(across(everything(), ~sum(., na.rm = T)))
  })) |> 
  unnest(df_pred)
gc()  


# df5$df_growth[[3]]

## b.USE THIS DIFFERENCE AND [dA] TO DERIVE PRACTICE GROWTH [dP] ---------

df6 <- df5 |>
  mutate(df_growth = pmap(
    list(df_growth, pred_2324, pred_1920),
    function(df, x, y, z) {
      df |>
        # mutate(dP = x - y - dD) |> 
        mutate(dP = x/y - 1) |> 
        # 6. [dC] RETURNING TO THE ORIGINAL EQUATION SOLVE FOR C --------------
      mutate(dC = (dT - dA - dP - dA*dP)/(1 + dA + dP + dA*dP)) |> 
        # ANYTHING LEFT IS INTERACTIONS TERMS ------------------------------
      mutate(dI =  dT - dA - dC - dP) |>
      # TO ENSURE FORWARDS COMPATIBILITY 
      mutate(dD = dA)
      # OR, SPECIFICALLY:
      # mutate(dI =  dC*dD + dD*dP +  dC*dP + dC*dP*dD)
    }
  ))

# df6$df_growth[[1]]


# 7. SAVE DF OF RESULTS FOR ODDS AND ATTRIBUTION --------------------------

df6 |>
  mutate(map_dfr(model, function(df) {
    df |>
      broom::tidy(parametric = TRUE) |>
      filter(term == "fyear2023/24") |>
      mutate(odds = exp(estimate)) |>
      mutate(lci = exp(estimate - 1.96*std.error)) |> 
      mutate(uci = exp(estimate + 1.96*std.error)) |> 
      select(odds, lci, uci)
      # pull(odds)
  }), .after = pdA) |> 
  unnest(df_growth) |> 
  # relocate(odds, .after = pdA) |> 
  # relocate(std_error, .after = odds) |> 
  select(-c(data, model, value, starts_with("pred_"))) |> 
  # ADJUSTMENTS IF SMALLER SAMPLE SIZE FOR MODELS 13:28 (FACTOR OR 0.35/0.17):
  # mutate(across(matches("^t"), ~ if_else(id %in% 13:28, . * (0.35 / 0.17), .))) %>%
  saveRDS(str_c("from_ncdr_growth_attrb_v4_", min(.$id), "to", max(.$id), ".rds"))



# APPENDIX ----------------------------------------------------------------
# some checks to understand growth
# DOESN'T ACTUALLY MATTER WHICH data[[x]] IS USED

# df3$data[[9]] |> 
#   group_by(fyear, sex) |> 
#   summarise(atts = n()) |> 
#   pivot_wider(names_from = "fyear",
#               values_from = "atts",
#               names_prefix = "atts") |> 
#   mutate(growth = (`atts2023/24` / `atts2019/20`) - 1)
# 
# 
# df3$data[[9]] |> 
#   mutate(age75plus = ifelse(age >= 75, 1, 0)) |> 
#   group_by(fyear, age75plus) |> 
#   summarise(atts = n()) |> 
#   pivot_wider(names_from = "fyear",
#               values_from = "atts",
#               names_prefix = "atts") |> 
#   mutate(growth = (`atts2023/24` / `atts2019/20`) - 1)
# 
# df3$data[[9]] |> 
#   group_by(fyear, imd_dec) |> 
#   summarise(atts = n()) |> 
#   pivot_wider(names_from = "fyear",
#               values_from = "atts",
#               names_prefix = "atts") |> 
#   mutate(growth = (`atts2023/24` / `atts2019/20`) - 1)
# 
# 
# df3$data[[9]] |> 
#   group_by(fyear, arr_mode) |> 
#   summarise(atts = n()) |> 
#   pivot_wider(names_from = "fyear",
#               values_from = "atts",
#               names_prefix = "atts") |> 
#   mutate(growth = (`atts2023/24` / `atts2019/20`) - 1)
# 
# 
# df3$data[[9]] |> 
#   group_by(fyear, acuity) |> 
#   summarise(atts = n()) |> 
#   pivot_wider(names_from = "fyear",
#               values_from = "atts",
#               names_prefix = "atts") |> 
#   mutate(growth = (`atts2023/24` / `atts2019/20`) - 1)
# 
# df3$data[[9]] |> 
#   group_by(fyear, inj_flag) |> 
#   summarise(atts = n()) |> 
#   pivot_wider(names_from = "fyear",
#               values_from = "atts",
#               names_prefix = "atts") |> 
#   mutate(growth = (`atts2023/24` / `atts2019/20`) - 1)
# 
# 
# df3$data[[9]] |> 
#   group_by(fyear, chief_comp_grp) |> 
#   summarise(atts = n()) |> 
#   pivot_wider(names_from = "fyear",
#               values_from = "atts",
#               names_prefix = "atts") |> 
#   mutate(growth = (`atts2023/24` / `atts2019/20`) - 1)
# 
# 
# df3$data[[9]] |> 
#   group_by(fyear, chief_comp_desc) |> 
#   summarise(atts = n()) |> 
#   pivot_wider(names_from = "fyear",
#               values_from = "atts",
#               names_prefix = "atts") |> 
#   mutate(growth = (`atts2023/24` / `atts2019/20`) - 1)
# 
# 
# 
# df3$data[[9]] |> 
#   group_by(fyear, chief_comp_desc) |> 
#   summarise(atts = n()) |> 
#   pivot_wider(names_from = "fyear",
#               values_from = "atts",
#               names_prefix = "atts") |> 
#   mutate(growth = (`atts2023/24` / `atts2019/20`) - 1)
# 
# df3$data[[9]] |> 
#   group_by(fyear, is_winter) |> 
#   summarise(atts = n()) |> 
#   pivot_wider(names_from = "fyear",
#               values_from = "atts",
#               names_prefix = "atts") |> 
#   mutate(growth = (`atts2023/24` / `atts2019/20`) - 1)
# 
# df3$data[[9]] |> 
#   group_by(fyear, is_wkend) |> 
#   summarise(atts = n()) |> 
#   pivot_wider(names_from = "fyear",
#               values_from = "atts",
#               names_prefix = "atts") |> 
#   mutate(growth = (`atts2023/24` / `atts2019/20`) - 1)
# 
# df3$data[[9]] |> 
#   group_by(fyear, is_night) |> 
#   summarise(atts = n()) |> 
#   pivot_wider(names_from = "fyear",
#               values_from = "atts",
#               names_prefix = "atts") |> 
#   mutate(growth = (`atts2023/24` / `atts2019/20`) - 1)
# 
# df3$data[[9]] |> 
#   group_by(fyear, procode) |> 
#   summarise(atts = n()) |> 
#   pivot_wider(names_from = "fyear",
#               values_from = "atts",
#               names_prefix = "atts") |> 
#   mutate(growth = (`atts2023/24` / `atts2019/20`) - 1)

# df3$data[[9]] |>
#   filter(!procode %in% c("RRK", "RNN")) |> 
#   group_by(fyear) |>
#   summarise(atts = n()) |>
#   pivot_wider(names_from = "fyear",
#               values_from = "atts",
#               names_prefix = "atts") |>
#   mutate(growth = (`atts2023/24` / `atts2019/20`) - 1)
