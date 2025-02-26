# README
# [NCDR]
# Select provider sample and extract data for Phase II.


# TODO: HAVE TO REVISIT DQ OF PROVIDER SAMPLE - 
# MISSINGNESS IS HIGHER THAN EXPECTED IN BED OCC SCRIPT


library("gt")
library("DBI")
library("here") 
library("dplyr")
library("purrr") 
library("readr") 
library("tidyr")
library("dbplyr")
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



# 1. ASSESS DATA QUAL AND SELECT PROVIDER SAMPLE ----------------------------

## a. data quality of ED "key" variables by provider ------------------------------------------

# SHARES FOUNDATIONAL QUERY WITH INDUSTRIAL ACTION WORK (FILTER FOR DESIRED YEAR):
tb_ecds_0 <- tbl(con_sandbox_su, in_schema("dbo", "1208_ind_action_model_extract_ecds_0"))

ecds_extract <- tb_ecds_0 |> 
  filter(fyear == "2023/24") |> 
  count(procode, chief_comp, acuity, invst_all) |>
  collect()

gc()

dq_providers_ed <- ecds_extract |>
  count(
    procode,
    na_inv = is.na(invst_all),
    na_chief = is.na(chief_comp),
    na_acuity = is.na(acuity),
    wt = n
  ) |> 
  group_by(procode) |> 
  mutate(p = round(n/sum(n), 4)) |>
  ungroup() |> 
  filter(if_all(starts_with("na"), ~ . == F)) |> 
  select(procode, p) |> 
  arrange(-p) |> 
  # FOR GRAPHIC IN PAST PROJ WE ILLUSTRATED NAs RATHER THAN COMPLETE RECORDS.
  # WILL KEEP THAT CONVENTION HERE:
  mutate(p = 1 - p) |> 
  filter(!is.na(p))

dq_providers_ed |> 
  print(n=50)

## b. data quality of APCS times by provider ------------------------------

dq_apc_times <- dbGetQuery(
  con_sus_plus,
  "
SELECT  
  LEFT(apcs.Der_Provider_Code, 3) AS procode,
  Admission_Method,
  times.Admission_Time,
  times.Discharge_Time,
  COUNT(*) AS n

FROM [NHSE_SUSPlus_Live].[dbo].[tbl_Data_SEM_APCS] apcs
  LEFT JOIN
  (
  SELECT DISTINCT
    APCS_Ident_min,
    Admission_Time,
    Discharge_Time
  FROM [NHSE_SUSPlus_Reporting].[Data].[PAT_Intermediate_Table_APC]
  WHERE (Discharge_Date >= '2023-01-01')
  ) times
  ON apcs.APCS_Ident = times.APCS_Ident_min

WHERE 1 = 1
  AND apcs.Der_Financial_Year IN ('2023/24')
  AND LEFT(apcs.Der_Provider_Code, 1) = 'R'

GROUP BY 
  LEFT(apcs.Der_Provider_Code, 3),
  Admission_Method,
  times.Admission_Time,
  times.Discharge_Time
	
"
) |>
  tibble()


dq_providers_apc <- dq_apc_times |> 
  # WHERE ADM AND DISCHARGE == 00:00:00 -
  # MARK THESE AS NA ADMISSION TIMES, AND SO PROCESS BELOW WILL EXCLUDE:
  mutate(Admission_Time = if_else(
    Admission_Time == "00:00:00" & Discharge_Time == "00:00:00", NA_character_ , Admission_Time
  )) |>
  mutate(na_time = if_else(is.na(Admission_Time) | is.na(Discharge_Time), TRUE, FALSE)) |>
  mutate(pod = case_when(
    str_detect(Admission_Method, "^1") ~ "planned",
    str_detect(Admission_Method, "^2") ~ "emergency",
    str_detect(Admission_Method, "^3|^8") ~ "non_emergency",
    TRUE ~ NA_character_
  )) |> 
  count(
    procode, 
    pod,
    na_time,
    wt = n
  )

dq_providers_apc <- dq_providers_apc |> 
  count(procode, na_time, wt = n) |> 
  group_by(procode) |> 
  mutate(p = round(n/sum(n), 4)) |>
  ungroup() |> 
  filter(if_all(starts_with("na"), ~ . == F)) |> 
  select(procode, p)|> 
  arrange(-p) |> 
  mutate(p = 1 - p) |> 
  filter(!is.na(p))


dq_providers_ed |> 
  arrange(-p)

## c. sensitivity -----------------------------------------------------

sens <-
  list(
    ed = dq_providers_ed,
    apc = dq_providers_apc
  ) |>
  tibble::enframe() |>
  pivot_wider(names_from = name, values_from = value) |>
  cross_join(tibble(ed_dq_level = seq(90, 80, by = -1))) |>
  cross_join(tibble(apc_dq_level = seq(99, 90, by = -1))) |> 
  mutate(n_providers = pmap_dbl(
    list(ed, apc, ed_dq_level, apc_dq_level),
    function(df1, df2, threshold1, threshold2) {
      df1 |>
        filter(
          # if_all(starts_with("p"), ~ . < ((100 - threshold1) / 100))
          p < ((100 - threshold1) / 100)
        ) |>
        semi_join(
          df2 |>
            filter(
              #   if_all(starts_with("p"), ~ . < ((100 - threshold2) / 100))
              p < ((100 - threshold2) / 100)
            ),
          join_by("procode")
        ) |>
        nrow()
    }
  ))

sens |>  count(n_providers)

# TODO: EVERYTHING BEYOND THIS POINT

# PLOTS (ALSO SEE TABLE BELOW)

# sens_2022to25 |> 
#   filter(between(apc_dq_level, 98, 99)) |> 
#   ggplot(aes(ed_dq_level, n_providers))+
#   geom_blank(aes(y=0))+
#   geom_point()+
#   facet_wrap(vars(apc_dq_level))

# sens_2022to25 |> 
#   filter(between(apc_dq_level, 97, 99)) |> 
#   ggplot(aes(ed_dq_level, apc_dq_level))+
#   geom_contour_filled(
#     aes(z = n_providers),
#     alpha = 0.8
#     )+
#   theme_minimal()+
#   scale_x_continuous(breaks = seq(80, 90, by = 2))+
#   geom_point(colour = "black")+
#   ylim(96, 100)+
#   labs(
#     x = "ED key vars completion (%)",
#     y = "APC times completion threshold (%)"
#     )


sens_plus <- sens |> 
  filter(
    # ed_dq_level %in% 80:87 &
    apc_dq_level %in% 98:99
  ) |> 
  mutate(related_procodes = pmap(
    list(ed, apc, ed_dq_level, apc_dq_level),
    function(df1, df2, threshold1, threshold2) {
      df1 |>
        filter(
          # if_all(starts_with("x"), ~ . < ((100 - threshold1) / 100))
          p < ((100 - threshold1) / 100)
          
        ) |>
        semi_join(
          df2 |>
            filter(
              # if_all(starts_with("x"), ~ . < ((100 - threshold2) / 100))
              p < ((100 - threshold2) / 100)
              
            ),
          join_by("procode")
        ) |>
        select(procode) |> 
        # str_flatten(collapse = ", ")
        identity()
    }
  )) |> 
  select(-c(1,2))


## d. representation stats ------------------------------------------------

# SHARES QUERY WITH INDUSTRIAL ACTION WORK (FILTER FOR DESIRED YEAR):
dq_provider_represent <- 
  tbl(
    con_sandbox_su, 
    in_schema("dbo", "1208_ind_action_representation_stats")) |> 
  filter(fyear == "2023/24") |> 
  collect() |> 
  clean_names() 


# FOR REGION LKP:
eric_2223 <- read_csv("https://files.digital.nhs.uk/FB/BE3AC8/ERIC%20-%20202223%20-%20Trust%20data.csv") |> 
  clean_names() |> 
  mutate(region = str_remove_all(commissioning_region, " COMMISSIONING REGION")) |> 
  select(trust_code, trust_name, region, trust_type)

# SELECT NECESSARY SUBSETS:

dq_provider_represent_t3 <- dq_provider_represent |>
  filter(na_arrival == 0) |> 
  # WHERE CLAUSE FLAGS:
  filter(if_all(starts_with("where"), ~ . == 1)| is_type3 == 1) |> 
  select(-na_arrival)|> # , starts_with("where")
  left_join(eric_2223, join_by(procode == trust_code))

dq_provider_represent <- dq_provider_represent_t3 |> 
  filter(if_all(starts_with("where"), ~ . == 1))

###

provider_popn <- dq_provider_represent |> 
  count(fyear, procode, wt = n) |> 
  # MORE THAN AV OF 15 A DAY FOR A YEAR:
  filter(n > 15*365) |> 
  # AND SUBMIT RECORDS IN ALL YEARS:
  complete(fyear, procode) |> 
  group_by(procode) |>
  filter(!is.na(sum(n))) |>
  ungroup() |>
  distinct(procode)

# TODO: THIS IS WHERE PARAMETERS ARE CHOSEN:
provider_selection <- sens_plus |> 
  filter(ed_dq_level == 90 & apc_dq_level== 99) |>
  select(related_procodes) |> 
  unnest(related_procodes)

###

provider_selection |> 
  left_join(eric_2223, join_by(procode == trust_code)) |> select(1,2) |> 
  rename(Provider_Code = procode, Provider_Name = trust_name) |> 
  arrange(Provider_Code) |> 
  mutate(id = row_number(), .before = Provider_Code) |> 
  mutate(Provider_Name = str_to_title(Provider_Name)) |> 
  mutate(Provider_Name = str_replace_all(Provider_Name, "Nhs", "NHS")) |> 
  print(n=35)

###

table_representation <- tibble(
  data = list(
    dq_provider_represent |>
      semi_join(provider_popn, join_by(procode)),
    dq_provider_represent |>
      semi_join(provider_selection, join_by(procode))
  )
) |>
  mutate(data_t3 = list(
    dq_provider_represent_t3 |>
      semi_join(provider_popn, join_by(procode)),
    dq_provider_represent_t3 |>
      semi_join(provider_selection, join_by(procode))
  )) |> 
  mutate(n_providers = map_dbl(data, \(df)
                               df |>
                                 count(procode) |>
                                 nrow()
  )) |>
  mutate(size = map(data, \(df)
                    df |>
                      filter(fyear == "2023/24") |>
                      count(fyear, procode, wt = n) |>
                      group_by(procode) |>
                      # MEAN OF A PROVIDER'S SIZE OVER 1 YEARS:
                      reframe(mean_n = mean(n)) |>
                      mutate(size_group = case_when(
                        mean_n < 70e3 ~ "attendances < 70k",
                        mean_n >= 70e3 & mean_n < 95e3 ~ "attendances 70-94k",
                        mean_n >= 95e3 & mean_n < 130e3 ~ "attendances 95-129k",
                        mean_n >= 130e3 ~ "attendances > 130k",
                        T ~ NA_character_
                      )) |>
                      mutate(size_group = factor(
                        size_group,
                        levels = c("attendances < 70k", "attendances 70-94k", "attendances 95-129k", "attendances > 130k")
                      )) |>
                      count(size_group) |>
                      mutate(p = n / sum(n)) |>
                      select(-n) |>
                      pivot_wider(names_from = size_group, values_from = p)
                    
  )) |>
  mutate(arrive_ambulance = map_dbl(data, \(df)
                                    df |>
                                      count(fyear, is_ambulance, wt = n) |>
                                      # filter(fyear != "2023/24") |>
                                      group_by(fyear) |>
                                      mutate(p = n / sum(n)) |>
                                      ungroup() |>
                                      filter(is_ambulance == 1) |>
                                      summarise(av = round(mean(p), 3)) |>
                                      pull(av))) |>
  mutate(admitted = map_dbl(data, \(df)
                            df |>
                              count(fyear, is_adm, wt = n) |>
                              # filter(fyear == "2023/24") |>
                              group_by(fyear) |>
                              mutate(p = n / sum(n)) |>
                              ungroup() |>
                              filter(is_adm == 1) |>
                              summarise(av = round(mean(p), 3)) |>
                              pull(av))) |>
  mutate(age_under_18 = map_dbl(data, \(df)
                                df |>
                                  count(fyear, age_under_18, wt = n) |>
                                  group_by(fyear) |>
                                  mutate(p = n / sum(n)) |>
                                  ungroup() |>
                                  filter(age_under_18 == 1) |>
                                  summarise(av = round(mean(p), 3)) |>
                                  pull(av))) |>
  mutate(age_75_plus = map_dbl(data, \(df)
                               df |>
                                 count(fyear, age_75_plus, wt = n) |>
                                 group_by(fyear) |>
                                 mutate(p = n / sum(n)) |>
                                 ungroup() |>
                                 filter(age_75_plus == 1) |>
                                 summarise(av = round(mean(p), 3)) |>
                                 pull(av))) |>
  mutate(urban = map_dbl(data, \(df)
                         df |>
                           count(fyear, is_urban, wt = n) |>
                           # BECAUSE LOTS OF NAS IN 2023/24:
                           filter(fyear == "2023/24") |>
                           group_by(fyear) |>
                           mutate(p = n / sum(n)) |>
                           ungroup() |>
                           filter(is_urban == 1) |>
                           summarise(av = round(mean(p), 3)) |>
                           pull(av))) |>
  mutate(deprived_20pc = map_dbl(data, \(df)
                                 df |>
                                   count(fyear, imd_quint_1, wt = n) |>
                                   group_by(fyear) |>
                                   mutate(p = n / sum(n)) |>
                                   ungroup() |>
                                   filter(imd_quint_1 == 1) |>
                                   summarise(av = round(mean(p), 3)) |>
                                   pull(av))) |>
  mutate(region = map(data, \(df)
                      df |>
                        count(fyear, region, wt = n, sort = T) |>
                        group_by(fyear) |>
                        mutate(p = n / sum(n)) |>
                        group_by(region) |>
                        summarise(av = round(mean(p), 3)) |>
                        ungroup() |>
                        pivot_wider(names_from = region, values_from = av) |>
                        clean_names() |>
                        rename_with(~ paste0("region_", .x, recycle0 = TRUE)) |>
                        identity())) |>
  mutate(also_offers_type_3 = map_dbl(data_t3, \(df)
                                      df |>
                                        count(fyear, procode, is_type3, wt = n) |>
                                        filter(fyear == "2023/24") |> 
                                        count(is_type3) |> 
                                        summarise(p = round(n[[2]]/n[[1]], 3)) |> 
                                        pull(p)
  )) |>
  
  mutate(four_hour = map(data, \(df)
                         df |> 
                           filter(fyear == "2023/24") |> 
                           count(procode, under_4hrs, wt = n) |> 
                           group_by(procode) |> 
                           mutate(p = n/sum(n)) |> 
                           ungroup() |> 
                           filter(under_4hrs ==1) |> 
                           arrange(p) |> 
                           # mutate(grp = case_when(
                           #   p < 0.48 ~ "4h wait standard < 48%",
                           #   p >= 0.48 & p < 0.58 ~ "4h wait standard 48%-58%",
                           #   p > 0.58  ~ "4h wait standard >= 58%",
                           #   TRUE ~ NA_character_
                           # )) |> 
                           # mutate(grp = factor(
                           #   grp,
                           #   levels = c(
                           #     "4h wait standard < 40%",
                           #     "4h wait standard 40%-59%",
                           #     "4h wait standard >= 60%"
                           #   )
                           # )) |>
                           mutate(grp = case_when(
                             p < 0.46 ~ "4h target < 46%",
                             p >= 0.46 & p < .58 ~ "4h target 46%-57%",
                             p >= 0.58 ~ "4h target >= 58%",
                             TRUE ~ NA_character_
                           )) |>
                           mutate(grp = factor(
                             grp,
                             levels = c(
                               "4h target < 46%",
                               "4h target 46%-57%",
                               "4h target >= 58%"
                             )
                           )) |>
                           count(grp) |> 
                           mutate(p = n / sum(n)) |>
                           select(-n) |>
                           pivot_wider(names_from = grp, values_from = p)
  )) |>
  mutate(twelve_hour = map(data, \(df)
                           df |> 
                             filter(fyear == "2023/24") |> 
                             count(procode, under_12hrs, wt = n) |> 
                             group_by(procode) |> 
                             mutate(p = n/sum(n)) |> 
                             ungroup() |> 
                             filter(under_12hrs == 1) |> 
                             arrange(p) |> 
                             mutate(grp = case_when(
                               p < 0.86 ~ "0-12h waits < 86%",
                               p >= 0.86 & p < 0.93 ~ "0-12h waits 86-92%",
                               p >= 0.93  ~ "0-12h waits >= 93%",
                               TRUE ~ NA_character_
                             )) |>
                             mutate(grp = factor(
                               grp,
                               levels = c(
                                 "0-12h waits < 86%",
                                 "0-12h waits 86-92%",
                                 "0-12h waits >= 93%"
                               )
                             )) |>
                             count(grp) |> 
                             mutate(p = n / sum(n)) |> 
                             select(-n) |>
                             pivot_wider(names_from = grp, values_from = p)
  )) |>
  unnest(size) |>
  unnest(region) |>
  unnest(four_hour) |>
  unnest(twelve_hour) |>
  select(-c(data, data_t3)) |>
  mutate(zz = if_else(n_providers > 50, "population", "sample"), .before = n_providers) |>
  pivot_longer(cols = !starts_with("zz"), names_to = "variable") |>
  pivot_wider(names_from = zz, values_from = value) |> 
  select(variable, population, sample) |> 
  mutate(across(2:3, ~if_else(is.na(.), 0, .))) |> 
  rowwise() |> 
  mutate(diff = sample - population)

table_representation |> 
  gt() |> 
  opt_row_striping(row_striping = F) |> 
  fmt_auto() |> 
  tab_style(
    style = list(
      cell_text(transform = "capitalize", weight = "bold")
    ),
    # different location
    locations = cells_column_labels(everything())
  ) |> 
  fmt_percent(rows = c(2:25), decimals = 1) |>
  tab_row_group(
    label = "Patient demographics:",
    rows =  str_detect(variable, "age|urban|depriv|region_")
  ) |> 
  tab_row_group(
    label = "Case-mix-related:",
    rows =  str_detect(variable, "arrive_|admitted")
  ) |> 
  tab_row_group(
    label = "Provider-related (23/24):",
    rows =  str_detect(variable, "att|also|4h|12h")
  ) |> 
  tab_row_group(
    label = "",
    rows =  str_detect(variable, "n_prov|23")
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups(groups = c(1, 2, 3, 4))
  ) |> 
  sub_values(values = -98, replacement = "") |> 
  gt_highlight_cols(sample, fill = "grey", alpha = 0.1) |> 
  tab_options(
    data_row.padding = px(1),
    table.font.size = "small"
  )


vec_provider_selection <- provider_selection |>
  pull() 

# ecds_extract_1 <- tb_ecds_0 |>
#   filter(procode %in% local(vec_provider_selection)) |> 
#   collect()

gc()

# OR TO PASTE IN QUERY:
provider_selection |>
  # mutate(procode = str_c("\'", procode, "\'")) |>
  mutate(procode = str_c("\"", procode, "\"")) |>
  pull() |>
  str_flatten_comma() |>
  cat()

# 'RKB', 'RCB', 'RHM', 'RCU', 'RVJ', 'RWW', 'RK9', 'RWH', 'RLT', 'RFF', 'RVW', 'REM', 'RBK', 'RTF', 'RX1', 'RYR', 'RQW', 'RWD', 'RYJ', 'RNN', 'RNZ', 'RTG', 'RHQ', 'RXC', 'RFS'
# "RKB", "RCB", "RHM", "RCU", "RVJ", "RWW", "RK9", "RWH", "RLT", "RFF", "RVW", "REM", "RBK", "RTF", "RX1", "RYR", "RQW", "RWD", "RYJ", "RNN", "RNZ", "RTG", "RHQ", "RXC", "RFS"

### 'RKB', 'RWJ', 'RCB', 'RHM', 'RCU', 'RVJ', 'RWA', 'RJC', 'RWW', 'RK9', 'RWH', 'RLT', 'RFF', 'RVW', 'REM', 'RBK', 'RTF', 'RX1', 'RYR', 'RQW', 'RWD', 'RYJ', 'RXF', 'RNN', 'RNZ', 'RTG', 'RHQ', 'RCD', 'RXC', 'RFS'
### "RKB", "RWJ", "RCB", "RHM", "RCU", "RVJ", "RWA", "RJC", "RWW", "RK9", "RWH", "RLT", "RFF", "RVW", "REM", "RBK", "RTF", "RX1", "RYR", "RQW", "RWD", "RYJ", "RXF", "RNN", "RNZ", "RTG", "RHQ", "RCD", "RXC", "RFS"