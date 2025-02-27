# README
# [NCDR]
# Calculate bed occupancy for providers in our sample, 
# for every hour over the study period.

library("gt")
library("DBI")
library("VIM")
library("here") 
library("dplyr")
library("purrr") 
library("furrr") 
library("readr") 
library("tidyr")
library("dbplyr")
library("tibble") 
library("ggplot2") 
library("janitor")
library("stringr")
library("gtExtras")
library("lubridate")


# 0. CONNECTIONS ----------------------------------------------------------

con_sandbox_su <- dbConnect(
  odbc::odbc(),
  Driver = "SQL Server",
  Server = "PRODNHSESQL101",
  Database = "NHSE_Sandbox_StrategyUnit",
  Trusted_Connection = "True"
)
con_sus_plus <- dbConnect(
  odbc::odbc(),
  Driver = "SQL Server",
  Server = "PRODNHSESQL101",
  Database = "NHSE_SUSPlus_Live",
  Trusted_Connection = "True"
)
con_reporting <- dbConnect(
  odbc::odbc(),
  Driver = "SQL Server",
  Server = "PRODNHSESQL101",
  Database = "NHSE_SUSPlus_Reporting",
  Trusted_Connection = "True"
)

# 1. GET DATA FROM SQL ----------------------------------------------------------
# CREATE DF OF PROVIDER ADM AND DIS TIMES, PLUS OTHER IMPUTATION VARS 
# FOR SELECTED PROVIDERS.

query_apcs <- "
SELECT apcs.Der_Financial_Year,
  LEFT(apcs.Der_Provider_Code, 3) AS procode,
  CASE
    WHEN LEFT(Admission_Method, 1) = '1'
    AND Patient_Classification = '1' THEN 'elec_ord'
    WHEN LEFT(Admission_Method, 1) = '1'
    AND Patient_Classification = '2' THEN 'elec_dc'
    WHEN LEFT(Admission_Method, 1) = '1'
    AND Patient_Classification IN ('3', '4') THEN 'elec_reg_dn'
    WHEN LEFT(Admission_Method, 1) = '2' THEN 'emer'
    WHEN LEFT(Admission_Method, 1) = '3' THEN 'mat'
    ELSE 'other'
  END as Admission_Method_min,
  Admission_Date,
  Discharge_Date,
  der_spell_los,
  times.Admission_Time,
  times.Discharge_Time
FROM [NHSE_SUSPlus_Live].[dbo].[tbl_Data_SEM_APCS] apcs
  LEFT JOIN (
    SELECT DISTINCT APCS_Ident_min,
      Admission_Time,
      Discharge_Time
    FROM [NHSE_SUSPlus_Reporting].[Data].[PAT_Intermediate_Table_APC]
  ) times ON apcs.APCS_Ident = times.APCS_Ident_min
WHERE 1 = 1
  AND apcs.Der_Financial_Year IN ('2022/23', '2023/24', '2024/25')
  AND (
    Discharge_Date >= '2023-04-01'
    OR Discharge_Date is NULL
  )
  AND Admission_Date <= '2024-04-01'
  AND LEFT(Der_Provider_Code, 3) IN (
    'RKB', 'RCB', 'RHM', 'RCU', 'RVJ',
    'RWW', 'RK9', 'RWH', 'RLT', 'RFF',
    'RVW', 'REM', 'RBK', 'RTF', 'RX1',
    'RYR', 'RQW', 'RWD', 'RYJ', 'RNN',
    'RNZ', 'RTG', 'RHQ', 'RXC', 'RFS'
    --'RKB', 'RWJ', 'RCB', 'RHM', 'RCU', 'RVJ',
    --'RWA', 'RJC', 'RWW', 'RK9', 'RWH', 'RLT',
    --'RFF', 'RVW', 'REM', 'RBK', 'RTF', 'RX1',
    --'RYR', 'RQW', 'RWD', 'RYJ', 'RXF', 'RNN',
    --'RNZ', 'RTG', 'RHQ', 'RCD', 'RXC', 'RFS'
    )
"

df_raw_apcs_times <- dbGetQuery(con_sus_plus, query_apcs) |> 
  tibble()

# df_raw_apcs_times |>
#   filter(is.na(Admission_Time) & is.na(Discharge_Time)) |>
# #   # filter(Der_Financial_Year == "2023/24") |> 
#   count(procode,  sort = T)
# #   count(procode, is.na(Admission_Time) & is.na(Discharge_Time), sort = T)
#   

df_apcs_times <- df_raw_apcs_times |> 
  # set null discharge times to some date well past 2024-07-31
  mutate(Discharge_Date = if_else(
    is.na(Discharge_Date),
    ymd("2030-01-01"),
    Discharge_Date
  )) |>
  mutate(Discharge_Time = if_else(
    is.na(Discharge_Date),
    "12:00:00",
    Discharge_Time
  )) |>
  # don't care about discharge times if after 2024-07-31
  mutate(Discharge_Time = if_else(
    Discharge_Date > "2024-07-31" & is.na(Discharge_Time),
    "12:00:00",
    Discharge_Time
  )) |>
  # infer admission date from discharge date and los
  mutate(Admission_Date = if_else(
    is.na(Admission_Date),
    Discharge_Date - days(der_spell_los),
    Admission_Date
  )) |>
  mutate(
    admi_wkend = if_else(wday(Admission_Date, week_start = 1) <= 5, 0, 1),
    disc_wkend = if_else(wday(Discharge_Date, week_start = 1) <= 5, 0, 1)
  ) |>
  mutate(
    Admission_Date_days = as.integer(ymd(Admission_Date) - ymd("2023-01-01")),
    Discharge_Date_days = as.integer(ymd(Discharge_Date) - ymd("2023-01-01"))
  ) |>
  mutate(
    Admission_Time_hms = hms(Admission_Time),
    Discharge_Time_hms = hms(Discharge_Time)
  ) |>
  mutate(
    Admission_Time_sec = period_to_seconds(Admission_Time_hms),
    Discharge_Time_sec = period_to_seconds(Discharge_Time_hms)
  ) |>
  mutate(same_day = ifelse(der_spell_los == 0, 1, 0)) |>
  mutate(at_dt_status = case_when(
    same_day == 1 &
      Admission_Time_sec > Discharge_Time_sec ~ "at_dt_invalid",
    is.na(Admission_Time) &
      !is.na(Discharge_Time) &
      Discharge_Time != "00:00:00" ~ "at_missing_only",
    is.na(Discharge_Time) &
      !is.na(Admission_Time) &
      Admission_Time != "00:00:00" ~ "dt_missing_only",
    is.na(Admission_Time) &
      is.na(Discharge_Time) ~ "at_dt_missing",
    Discharge_Time == "00:00:00" &
      Admission_Time == "00:00:00" ~ "at_dt_00",
    TRUE ~ "at_dt_complete"
  )) |>
  # set admission and discharge times to NA if invalid so they are picked up by imputation process
  mutate(
    Admission_Time_sec = ifelse(at_dt_status == "at_dt_invalid", NA, Admission_Time_sec),
    Discharge_Time_sec = ifelse(at_dt_status == "at_dt_invalid", NA, Discharge_Time_sec),
    Admission_Time_sec = ifelse(at_dt_status == "at_dt_00", NA, Admission_Time_sec),
    Discharge_Time_sec = ifelse(at_dt_status == "at_dt_00", NA, Discharge_Time_sec)
  ) |> 
  clean_names()

# 2. CHECK MISSINGNESS OF TIMES ----------------------------------------------------------

df_apcs_times |> 
  # filter(der_financial_year == "2023/24") |>
  group_by(at_dt_status, admission_method_min) |> 
  summarise(n = n()) |> 
  ungroup() |> 
  group_by(admission_method_min) |> 
  mutate(p = n / sum(n, na.rm = TRUE)) |> 
  ungroup() |> 
  select(-p) |> 
  pivot_wider(
    names_from = "at_dt_status", 
    values_from = "n",
    values_fill = 0
  )

# apc_times_imputation_df |> 
#   filter(admission_method_min == "emer") |> 
#   group_by(admi_wkend) |> 
#   summarise(n = n())
# 
# apc_times_imputation_df |> 
#   filter(admission_method_min == "emer") |> 
#   group_by(disc_wkend) |> 
#   summarise(n = n())

# apc_times_imputation_df |>
#   filter(admission_method_min == "emer") |>
#   group_by(los) |>
#   summarise(n = n()) |> 
#   arrange(-n)


# 3. IMPUTE MISSING TIMES  ----------------------------------------------------------

# IMPUTE USING kNN

provider_selection <- c(
  # FROM DATA QUALITY ASSESSMENT:
  "RKB", "RCB", "RHM", "RCU", "RVJ",
  "RWW", "RK9", "RWH", "RLT", "RFF", 
  "RVW", "REM", "RBK", "RTF", "RX1", 
  "RYR", "RQW", "RWD", "RYJ", "RNN", 
  "RNZ", "RTG", "RHQ", "RXC", "RFS"
  # "RKB", "RWJ", "RCB", "RHM", "RCU", "RVJ",
  # "RWA", "RJC", "RWW", "RK9", "RWH", "RLT", 
  # "RFF", "RVW", "REM", "RBK", "RTF", "RX1",
  # "RYR", "RQW", "RWD", "RYJ", "RXF", "RNN",
  # "RNZ", "RTG", "RHQ", "RCD", "RXC", "RFS"
) |> 
  enframe(name = NULL, value = "procode")

df_admi_meth <- df_apcs_times |> distinct(admission_method_min)

# PREPARE BASE DF (RUNTIME: ~ 3 MINS):
df_impute_prep <- cross_join(
  provider_selection,
  df_admi_meth
) |>
  cross_join(
    tibble(admi_wkend = c(0, 1))
  ) |>
  # SELECT DESIRED SUBSETS FROM MAIN DATAFRAME: 
  mutate(data = pmap(
    list(procode, admission_method_min, admi_wkend),
    function(x, y, z) {
      df_apcs_times |>
        filter(procode == x) |>
        filter(admission_method_min == y) |>
        filter(admi_wkend == z)
    }
  ))

gc()


# RUNTIME: <= 7 MINS:
tictoc::tic()
set.seed(1337)
plan(multisession, workers = 4)

df_impute <- df_impute_prep |> 
  ###
  ### 1.
  mutate(missing_at_only_nsd = future_map(
    data, function(df) {
      df |> 
        filter(same_day == 0) |> 
        filter(at_dt_status %in% c("at_missing_only", "at_dt_complete")) |> 
        kNN(
          variable = "admission_time_sec",
          k = 1,
          dist_var = c("admission_date_days", "der_spell_los"),
          addRandom = TRUE
        ) |> 
        filter(admission_time_sec_imp == TRUE) |> 
        select(-admission_time_sec_imp) |> 
        as_tibble()
    }
  ))  |> 
  ###
  ### 2.
  mutate(missing_at_only_sd = future_map(data, function(df) {
    df |> 
      filter(same_day == 1) |> 
      filter(at_dt_status %in% c("at_missing_only", "at_dt_complete")) |> 
      kNN(
        variable = "admission_time_sec",
        k = 1,
        dist_var = c("admission_date_days", "discharge_time_sec"),
        addRandom = TRUE
      ) |> 
      filter(admission_time_sec_imp == TRUE) |> 
      select(-admission_time_sec_imp) |> 
      as_tibble()
  }
  )) |> 
  ###
  ### 3.
  mutate(missing_dt_only_nsd = future_map(
    data, function(df) {
      df |> 
        filter(same_day == 0) |> 
        filter(at_dt_status %in% c("dt_missing_only", "at_dt_complete")) |> 
        kNN(
          variable = "discharge_time_sec",
          k = 1,
          dist_var = c("discharge_date_days", "der_spell_los"),
          addRandom = TRUE
        ) |> 
        filter(discharge_time_sec_imp == TRUE) |> 
        select(-discharge_time_sec_imp) |> 
        as_tibble()
    }
  )) |> 
  ###
  ### 4.
  mutate(missing_dt_only_sd = future_map(
    data, function(df) {
      df |> 
        filter(same_day == 1) |> 
        filter(at_dt_status %in% c("dt_missing_only", "at_dt_complete")) |> 
        kNN(
          variable = "discharge_time_sec",
          k = 1,
          dist_var = c("discharge_date_days", "admission_time_sec"),
          addRandom = TRUE
        ) |> 
        filter(discharge_time_sec_imp == TRUE) |> 
        select(-discharge_time_sec_imp) |> 
        as_tibble()
      
    }
  )) |> 
  ###
  ### 5.
  mutate(missing_at_dt_nsd = future_map(data, function(df) {
    df |> 
      filter(same_day == 0) |> 
      filter(at_dt_status %in% c(
        "at_dt_missing", "at_dt_invalid",
        "at_dt_00","at_dt_complete"
      )) |> 
      kNN(
        variable = "admission_time_sec",
        k = 1,
        dist_var = c("admission_date_days", "der_spell_los"),
        addRandom = TRUE
      ) |> 
      kNN(
        variable = "discharge_time_sec",
        k = 1,
        dist_var = c("discharge_date_days", "der_spell_los"),
        addRandom = TRUE
      ) |> 
      filter(admission_time_sec_imp == TRUE | discharge_time_sec_imp == TRUE) |> 
      select(-admission_time_sec_imp, -discharge_time_sec_imp) |> 
      as_tibble()
  }
  )) |> 
  ###
  ### 6.
  mutate(missing_at_dt_sd = future_map(
    data, function(df) {
      df |> 
        filter(same_day == 1) |> 
        filter(at_dt_status %in% c(
          "at_dt_missing", "at_dt_invalid",
          "at_dt_00","at_dt_complete"
        )) |> 
        kNN(
          variable = "admission_time_sec",
          k = 1,
          dist_var = c("admission_date_days"),
          addRandom = TRUE
        ) |> 
        kNN(
          variable = "discharge_time_sec",
          k = 1,
          dist_var = c("admission_time_sec"),
          addRandom = TRUE
        ) |> 
        filter(admission_time_sec_imp == TRUE | discharge_time_sec_imp == TRUE) |> 
        select(-admission_time_sec_imp, -discharge_time_sec_imp) |> 
        as_tibble()
      
    }
  ))

tictoc::toc()

df_imputed_only <- df_impute |> 
  select(starts_with("missing")) |> 
  pivot_longer(cols = everything()) |> 
  select(-name) |> 
  unnest(cols = value) |> 
  mutate(discharge_time_sec = if_else(is.na(discharge_date), NA, discharge_time_sec))


# 4. IMPUTATION CHECKS ----------------------------------------------------------

# check no admissions after discharges
df_imputed_only |> 
  # filter(der_financial_year == "2024/25") |>
  filter(der_financial_year == "2023/24") |>
  filter(
    admission_date_days == discharge_date_days & 
      admission_time_sec > discharge_time_sec
  ) |> 
  select(
    at_dt_status, same_day,
    admission_date_days, discharge_date_days,
    admission_time_sec, discharge_time_sec
  )
# identity() |> 
# select(
#   procode, admission_method_min,
#   at_dt_status, same_day,
#   admi_wkend,
#   disc_wkend,
#   der_spell_los,
#   admission_date_days, discharge_date_days,
#   admission_time_sec, discharge_time_sec
#   ) |> 
# view("")
# MEAN FOR PROVIDER

# visual imputed values follow similar distribution as non-imputed values

admi_wkend_names = c(`0` = "weekend admission", `1` = "weekday admission")
disc_wkend_names = c(`0` = "weekend discharge", `1` = "weekday discharge")
same_day_names = c(`0` = "overnight spell", `1` = "same day spell")


ggplot() +
  geom_density(data = subset(df_apcs_times, !is.na(admi_wkend)),
               aes(x = admission_time_sec),
               colour = 'blue') +
  geom_density(data = subset(df_imputed_only, !is.na(admi_wkend)),
               aes(x = admission_time_sec),
               colour = 'red') +
  facet_grid(rows = vars(admission_method_min),
             cols = vars(same_day, admi_wkend),
             labeller = labeller(same_day = as_labeller(same_day_names),
                                 admi_wkend = as_labeller(admi_wkend_names))) +
  scale_x_continuous(name = "hour admission (blue known |red imputed)",
                     label = scales::label_number(scale = 1/3600),
                     breaks = c(0, 21600, 43200, 64800, 86400))


ggplot() +
  geom_density(data = subset(df_apcs_times, !is.na(disc_wkend)),
               aes(x = discharge_time_sec),
               colour = 'blue') +
  geom_density(data = subset(df_imputed_only, !is.na(disc_wkend)),
               aes(x = discharge_time_sec),
               colour = 'red') +
  facet_grid(rows = vars(admission_method_min),
             cols = vars(same_day, disc_wkend),
             labeller = labeller(same_day = as_labeller(same_day_names),
                                 disc_wkend = as_labeller(disc_wkend_names))) +
  scale_x_continuous(name = "hour of discharge (blue known |red imputed)",
                     label = scales::label_number(scale = 1/3600),
                     breaks = c(0, 21600, 43200, 64800, 86400))



# 5. COMBINE KNOWN AND IMPUTED VALUES ---------------------------------------

# df_apcs_times |> 
#   filter(at_dt_status != "at_dt_complete") |> 
#   count(at_dt_status, sort = T) 
# 
# df_imputed_only |> 
#   count(at_dt_status, sort = T)

df_apcs_times_imputed <- df_imputed_only |> 
  # filter(der_financial_year == "2023/24") |> 
  mutate(
    admission_time_hms = seconds_to_period(admission_time_sec),
    discharge_time_hms = seconds_to_period(discharge_time_sec)
  ) |>  
  bind_rows(
    df_apcs_times |> 
      # filter(der_financial_year == "2023/24") |>
      filter(at_dt_status == "at_dt_complete")
  ) |> 
  mutate(
    admission_datetime = admission_date + admission_time_hms,
    discharge_datetime = discharge_date + discharge_time_hms
  ) |> 
  select(procode, admission_datetime, discharge_datetime, admission_method_min, at_dt_status)


# 6. SAVE FILE --------------------------------------------------------------

saveRDS(df_apcs_times_imputed, "df_diagnostics_apcs_times_imputed.RDS")

gc()

# 7. CALCULATE OCCUPANCY -----------------------------------------------------
# by provider hour and day - at half past the hour

start_datetime <- as_datetime("2023-03-31 00:30:00")
end_datetime <- as_datetime("2024-04-04 23:30:00")

clock <- tibble(census_dttm = seq(start_datetime, end_datetime, by = "hours"))

df_bed_occupancy <- clock |> 
  left_join(
    df_apcs_times_imputed,
    join_by(
      census_dttm >= admission_datetime,
      census_dttm < discharge_datetime
    )
  ) |> 
  count(census_dttm, procode, admission_method_min, name = "ip_occ") |> 
  arrange(procode, admission_method_min, census_dttm) |> 
  mutate(
    year = year(census_dttm),
    month = month(census_dttm),
    day = day(census_dttm),
    hour = hour(census_dttm)
  )


# visual checks
df_bed_occupancy |>
  # EITHER PERIOD OF INTEREST:
  filter(between(date(census_dttm), as_date("2023-04-01"), as_date("2024-03-31"))) |>
  # OR CLOSER LOOK AT TRANSITIONS:
  # filter(date(census_dttm) <= as_date("2023-04-30")) |> 
  # filter(date(census_dttm) >= as_date("2024-03-01")) |> 
  group_by(census_dttm) |> 
  summarise(ip_occ = sum(ip_occ)) |> 
  ggplot() +
  geom_line(aes(x = census_dttm, y = ip_occ))+
  theme_minimal()+
  geom_blank(aes(y = 0))

df_bed_occupancy |> 
  filter(between(date(census_dttm), as_date("2023-04-01"), as_date("2024-03-31"))) |>
  group_by(census_dttm, admission_method_min) |> 
  summarise(ip_occ = sum(ip_occ)) |> 
  ggplot() +
  geom_line(aes(x = census_dttm, y = ip_occ)) +
  facet_wrap(vars(admission_method_min))+ # , ncol = 1
  theme_bw()+
  geom_blank(aes(y = 0))

df_bed_occupancy |> 
  filter(between(date(census_dttm), as_date("2023-04-01"), as_date("2024-03-31"))) |>
  group_by(census_dttm, procode) |> 
  summarise(ip_occ = sum(ip_occ)) |> 
  ggplot() +
  geom_line(aes(x = census_dttm, y = ip_occ)) +
  facet_wrap(vars(procode))+
  geom_blank(aes(y = 0))


# 8. SAVE FILE --------------------------------------------------------------

saveRDS(df_bed_occupancy, "df_diagnostics_bed_occupancy.RDS")


# 9. OCCUPANCY VAR --------------------------------------------------------
# TODO: WHAT TO DO ABOUT BANK HOLIDAYS / UNUSUAL DAYS / STRIKE DAYS??

tmpl <- df_bed_occupancy |> 
  filter(between(date(census_dttm), as_date("2023-04-01"), as_date("2024-03-31"))) |>
  mutate(wkday = lubridate::wday(census_dttm, week_start = 1, label = T)) |> 
  count(procode, year, month, day, wkday, hour, wt = ip_occ, name = "occ") |> 
  group_by(procode, wkday, hour) |> 
  mutate(mean_occ = mean(occ)) |> 
  ungroup() |> 
  mutate(occ_scaled = occ/mean_occ)

# tmpl |> 
#   slice_sample(n=10e3) |> 
#   ggplot()+
#   geom_histogram(aes(scale))

tmpl |> 
  mutate(occ_decile = ntile(scale, 10)) |> 
  count(occ_decile)

tmpl |> 
  slice_sample(prop =.2) |>
  ggplot()+
  geom_violin(aes(scale, procode, fill = procode))+
  # facet_wrap(vars(procode))+
  coord_flip()

library("ggbeeswarm")  

# TODO: vs
# when bed occ increased 20-30% over mean then were % likely
# when bed occ in highest quantile then were % likely

tmpl |> 
  mutate(occ_decile = ntile(scale, 10)) |> 
  select(procode, month, day, hour, scale, occ_decile) |> 
  filter(occ_decile == 1) |> 
  arrange(scale)
# TODO BANK HOLS AND HOLS (CHRISTMAS)/ UNUSUAL DAYS / STRIKE DAYS SHOULD BE EXCEPTIONS

tmpl |> 
  mutate(occ_decile = ntile(scale, 10)) |>
  group_by(occ_decile) |> 
  mutate(cut = min(scale)) |> 
  ungroup() |> 
  slice_sample(prop =.05) |>
  mutate(occ_decile = as.factor(occ_decile)) |> 
  ggplot()+
  # geom_jitter(aes("", scale), alpha = .02)+
  # ggbeeswarm::geom_quasirandom(aes("", scale), alpha = .02)+
  ggbeeswarm::geom_beeswarm(aes("", scale, col = occ_decile), alpha = 0.6)+
  geom_hline(aes(yintercept = cut))+
  scale_color_viridis_d()+
  # scale_color_discrete_qualitative()+
  # coord_flip()+
  facet_wrap(vars(procode), nrow = 1)+
  theme(legend.position = "bottom")

# TODO OR KEEP AS % RELATIVE TO MEAN - BUT GROUPS
# when bed occ increased 20-30% over mean then were % likely
