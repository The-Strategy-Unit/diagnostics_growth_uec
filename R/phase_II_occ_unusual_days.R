# README
# Data frame of unusual dates, with underlying data sourced
# from Strategy Unit "Industrial Action" project.

library("here") 
library("tidyr")
library("dbplyr")
library("tibble") 
library("janitor")
library("stringr")
library("lubridate")

df_unusual_dates <- list.files(pattern = "dates.rds") |>
  enframe() |>
  mutate(data = map(value, \(x) readRDS(x))) |>
  # SELECT PERIOD USED IN THIS PROJECT:
  mutate(data = map2(
    data, name,
    function(df, x) {
      if (x == 1) {
        df |>
          filter(
            between(
              date,
              lubridate::as_date("2023-04-01"),
              lubridate::as_date("2024-03-31")
            )
          ) |>
          select(date, strike_day_seq, strike_type)
      } else {
        df |>
          as_tibble() |>
          filter(!measure %in% c(
            "after strike",
            "ambulance strike SEAS",
            "nurse strike Unite members some trusts"
          )) |>
          filter(
            between(
              date,
              lubridate::as_date("2023-04-01"),
              lubridate::as_date("2024-03-31")
            )
          ) |>
          mutate(strike_day_seq = NA_character_) |>
          select(date, strike_day_seq, strike_type = measure)
      }
    }
  )) |> 
  select(-c(name, value)) |> 
  unnest(data) |> 
  arrange(date) |> 
  # REMOVE 2 DAYS BEFORE A STRIKE FROM LIST OF UNUSUAL DAYS:
  mutate(flag = if_else(str_detect(strike_day_seq, "^1"), 1, 0)) |> 
  mutate(flag = if_else(flag == 1, date - days(2), NA_Date_)) |>
  tidyr::fill(flag, .direction = "up") |> 
  filter(!(strike_type == "before strike" & date == flag)) |> 
  group_by(date) |> 
  # REMOVE "BEFORE A STRIKE" WHEN IT OCCURS ON ANOTHER UNUSUAL DAY:
  filter(!(sum(row_number())> 1 & strike_type == "before strike") ) |> 
  ungroup() |> 
  select(-flag) |> 
  arrange(strike_type) |> 
  group_by(date) |> 
  # IF TWO EVENTS ON SAME UNUSUAL DAY, SELECT ONE WITH BIGGEST IMPACT ON OCC:
  # filter(sum(row_number()) > 1) |> 
  filter(!row_number() > 1) |> 
  ungroup() |> 
  arrange(date) |> 
  mutate(year = year(date), month = month(date), day = day(date))
