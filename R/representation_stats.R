# README
# Load and tidy data used to illustrate data quality and
# provider representation in sample (for report appendix).

# Also includes code to examine SQL "WHERE clause" impact.

library("gt") 
library("here") 
library("purrr") 
library("dplyr")
library("readr")
library("tidyr")
library("ggplot2")
library("stringr")
library("janitor")
library("gtExtras") 
library("lubridate")

# 1. SAMPLE-1-RELATED -------------------------------------------------------

## a. load data sets -----------------------------------------------------

# FOR REGION LKP:
eric_2223 <- read_csv("https://files.digital.nhs.uk/FB/BE3AC8/ERIC%20-%20202223%20-%20Trust%20data.csv") |> 
  clean_names() |> 
  mutate(region = str_remove_all(commissioning_region, " COMMISSIONING REGION")) |> 
  select(trust_code, trust_name, region, trust_type)

dq_multicoding <- readRDS(
  here("data_raw", "from_ncdr_trends_240917_dq_multicoding.rds")
)
ts_provider_attds <- readRDS(
  here("data_raw", "from_ncdr_240917_ts_prov_att.rds")
)
dq_provider_na_24_99 <- readRDS(
  here("data_raw", "from_ncdr_trends_240917_dq_prov_na_24_99.rds")
)

### FROM PULL REPRESENTATION
dq_provider_represent <- readRDS(
  here("data_raw", "from_ncdr_250120_prov_represent.rds")
) 

### FROM PLOT TRENDS
df_preplot_trend <- readRDS(
  here("data", "from_ncdr_trends_241204_df_preplot.rds")
)


## b. representation options -------------------------------------------

dq_provider_represent_trend_t3 <- dq_provider_represent |>
  # WHERE CLAUSE FLAGS:
  filter(if_all(starts_with("where"), ~ . == 1)| is_type3 == 1) |> 
  # select(-starts_with("where")) |> 
  left_join(eric_2223, join_by(procode == trust_code))

dq_provider_represent_trend <- dq_provider_represent_trend_t3 |> 
  filter(if_all(starts_with("where"), ~ . == 1))

##

dq_provider_represent_ecds_only_t3 <- dq_provider_represent |>
  filter(fyear %in% c("2019/20", "2023/24")) |> 
  filter(na_arrival == 0) |> 
  filter(covid_exclusion == 0) |> 
  # WHERE CLAUSE FLAGS:
  filter(if_all(starts_with("where"), ~ . == 1)| is_type3 == 1) |> 
  select(-c(na_arrival, covid_exclusion))|> # , starts_with("where")
  left_join(eric_2223, join_by(procode == trust_code))

dq_provider_represent_ecds_only <- dq_provider_represent_ecds_only_t3 |> 
  filter(if_all(starts_with("where"), ~ . == 1))
  

## a. provider survival ------------------------------------------------

ts_provider_survival <-  ts_provider_attds |>
  count(fyear) 

providers_surviving <- ts_provider_attds |>
  # MORE THAN AV OF 15 A DAY FOR A YEAR (EXCLUDES RVR ANOMALY)
  filter(n > 15*365) |>
  # NOW CLOSED (FROM MID 2023):
  filter(procode != "RVY") |>
  complete(fyear, procode) |>
  group_by(procode) |>
  filter(!is.na(sum(n))) |>
  ungroup() |>
  distinct(procode)

## d. multicoding -------------------------------------------------------

dq_multicoding_sensitivity <- dq_multicoding |>
  cross_join(
    tibble(n_tests_of_type_in_record = c(2, 3, 4))
  ) |>
  cross_join(
    # MINIMUM PROPORTION OF CASES (AS PROPORTION OF 1-4) IN EACH YEAR:
    tibble(annual_instance_threshold = c(
      1 / 100000,
      1 / 50000,
      1 / 10000,
      1 / 5000,
      1 / 1000,
      1 / 500,
      1 / 100,
      1 / 50,
      1 / 10
    ))
  ) |>
  mutate(data = pmap(
    list(data, n_tests_of_type_in_record, annual_instance_threshold),
    \(df, y, z)
    df |>
      mutate(procode = str_sub(procode, 1, 3)) |> 
      filter(code_occurs == y) |>
      # WE WILL GET PRACTICE VARIATION ACROSS PROVIDERS, SO WHAT P DO WE PICK?
      # (% CASES IN ALL YEARS > z)
      filter(p > z) |>
      # NEXT FEW LINES ARE TO MAKE SURE ALL YEARS ACCOUNTED FOR:
      complete(fyear, procode) |>
      group_by(procode) |>
      filter(!is.na(sum(code_occurs))) |>
      ungroup()
  ))

dq_multicoding_provider_n <- dq_multicoding_sensitivity |>
  mutate(n_prov = map_vec(
    data,
    \(df)
    df |>
      distinct(procode) |>
      count() |>
      pull(n)
  )) |>
  mutate(n_prov_500 = map_vec(
    data,
    \(df)
    # MORE THAN 500 CASES EACH YEAR:
    df |>
      filter(n >= 500) |>
      complete(fyear, procode) |>
      group_by(procode) |>
      filter(!is.na(sum(code_occurs))) |>
      ungroup() |>
      count(procode) |>
      count() |>
      pull(n)
  )) |>
  mutate(n_prov_50 = map_vec(
    data,
    \(df)
    # MORE THAN 50 CASES EACH YEAR:
    df |>
      filter(n >= 50) |>
      complete(fyear, procode) |>
      group_by(procode) |>
      filter(!is.na(sum(code_occurs))) |>
      ungroup() |>
      count(procode) |>
      count() |>
      pull(n)
  )) |> 
  mutate(n_prov_20 = map_vec(
    data,
    \(df)
    # MORE THAN 20 CASES EACH YEAR:
    df |>
      filter(n >= 20) |>
      complete(fyear, procode) |>
      group_by(procode) |>
      filter(!is.na(sum(code_occurs))) |>
      ungroup() |>
      count(procode) |>
      count() |>
      pull(n)
  )) |> 
  mutate(n_prov_2 = map_vec(
    data,
    \(df)
    # MORE THAN 1 CASE EACH YEAR:
    df |>
      filter(n >= 2) |>
      complete(fyear, procode) |>
      group_by(procode) |>
      filter(!is.na(sum(code_occurs))) |>
      ungroup() |>
      count(procode) |>
      count() |>
      pull(n)
  )) |> 
  mutate(n_tests_of_type_in_record = as.character(n_tests_of_type_in_record)) |>
  mutate(invest_name = case_when(
    invest_code == "01" ~ "01 | X-ray",
    invest_code == "03" ~ "03 | Haematology",
    invest_code == "05" ~ "05 | Biochemistry",
    invest_code == "10" ~ "10 | Ultrasound",
    invest_code == "11" ~ "11 | MRI",
    invest_code == "12" ~ "12 | CT"
  ))

dq_multicoding_provider_lists_relaxed <- dq_multicoding_provider_n |> 
  filter(
    (invest_code == "01" & n_tests_of_type_in_record == 2 & annual_instance_threshold == 1/1000)|
      (invest_code == "03" & n_tests_of_type_in_record == 2 & annual_instance_threshold == 1/500)|
      (invest_code == "05" & n_tests_of_type_in_record == 2 & annual_instance_threshold == 1/100)|
      (invest_code == "12" & n_tests_of_type_in_record == 2 & annual_instance_threshold == 1/1000)
  ) |> 
  mutate(prov_list = map(
    data,
    \(df)
    df |> 
      complete(fyear, procode) |>
      group_by(procode) |>
      filter(!is.na(sum(code_occurs))) |>
      ungroup() |> 
      distinct(procode) 
  )) |> 
  mutate(prov_list_500 = map(
    data,
    \(df)
    df |> 
      filter(n >= 500) |> 
      complete(fyear, procode) |>
      group_by(procode) |>
      filter(!is.na(sum(code_occurs))) |>
      ungroup() |> 
      distinct(procode) 
  )) |> 
  mutate(prov_list_50 = map(
    data,
    \(df)
    df |> 
      filter(n >= 50) |> 
      complete(fyear, procode) |>
      group_by(procode) |>
      filter(!is.na(sum(code_occurs))) |>
      ungroup() |> 
      distinct(procode) 
  )) |> 
  mutate(prov_list_20 = map(
    data,
    \(df)
    df |> 
      filter(n >= 20) |> 
      complete(fyear, procode) |>
      group_by(procode) |>
      filter(!is.na(sum(code_occurs))) |>
      ungroup() |> 
      distinct(procode) 
  )) |> 
  mutate(prov_list_4 = map(
    data,
    \(df)
    df |> 
      filter(n >= 5) |> 
      complete(fyear, procode) |>
      group_by(procode) |>
      filter(!is.na(sum(code_occurs))) |>
      ungroup() |> 
      distinct(procode) 
  ))

providers_s1 <-
  semi_join(
    providers_surviving,
    reduce(dq_multicoding_provider_lists_relaxed$prov_list_4, semi_join),
    join_by(procode)
  )

# 2. SAMPLE-2-RELATED ---------------------------------------------------

## a. load data sets -----------------------------------------------------

dqual_ecds_only <- readRDS(
  here("data_raw", "from_ncdr_ecds_only_240917_dq_isna.RDS")
)
dqual_ecds_only_provider_key <- readRDS(
  here("data_raw", "from_ncdr_ecds_only_240917_dq_provider.RDS")
) |> 
  # NOT PROVIDERS WITH ARRIVAL MODE = NULL > 5% (SEE EDA ON NCDR DS SERVER):
  filter(!procode %in% c(
    "RCX",
    "RQM",
    "RJ7",
    "RAX",
    "RQX",
    "RVY",
    "RP5",
    "RRV"
  ))

## b. representation options -------------------------------------------

dq_provider_represent_ecds_only_t3 <- dq_provider_represent |>
  filter(fyear %in% c("2019/20", "2023/24")) |> 
  filter(na_arrival == 0) |> 
  filter(covid_exclusion == 0) |> 
  # WHERE CLAUSE FLAGS:
  filter(if_all(starts_with("where"), ~ . == 1)| is_type3 == 1) |> 
  select(-c(na_arrival, covid_exclusion))|> # , starts_with("where")
  left_join(eric_2223, join_by(procode == trust_code))

dq_provider_represent_ecds_only <- dq_provider_represent_ecds_only_t3 |> 
  filter(if_all(starts_with("where"), ~ . == 1))



## c. provider survival ---------------------------------------------------

providers_surviving_ecds_only <- ts_provider_attds |>
  # NOT PROVIDERS WITH ARRIVAL MODE = NULL > 5% (SEE EDA ON NCDR DS SERVER):
  filter(!procode %in% c(
    "RCX",
    "RQM",
    "RJ7",
    "RAX",
    "RQX",
    "RVY",
    "RP5",
    "RRV"
  )) |> 
  # MORE THAN AV OF 15 A DAY FOR A YEAR (EXCLUDES RVR ANOMALY):
  filter(n > 15*365) |>
  # NOW CLOSED (FROM MID 2023):
  filter(procode != "RVY") |>
  filter(fyear %in% c("2019/20", "2023/24")) |>
  complete(fyear, procode) |>
  group_by(procode) |>
  filter(!is.na(sum(n))) |>
  ungroup() |>
  distinct(procode)

providers_key_90 <- dqual_ecds_only_provider_key |> 
  # TO KEEP CONSISTENT WITH ABOVE CHANGE TO % NA:
  mutate(across(starts_with("x"), ~ 1 - .)) |> 
  filter(!if_any(starts_with("x"), ~ is.na(.))) |> 
  rowwise() |> 
  mutate(xmean = mean(c_across(x2019_20: x2023_24))) |> 
  ungroup() |> 
  filter(if_all(starts_with("x"), ~ . < .10)) 

providers_key_85 <- dqual_ecds_only_provider_key |> 
  # TO KEEP CONSISTENT WITH ABOVE CHANGE TO % NA:
  mutate(across(starts_with("x"), ~ 1 - .)) |> 
  filter(!if_any(starts_with("x"), ~ is.na(.))) |> 
  rowwise() |> 
  mutate(xmean = mean(c_across(x2019_20: x2023_24))) |> 
  ungroup() |> 
  filter(if_all(starts_with("x"), ~ . < .15)) |> 
  anti_join(
    providers_key_90, join_by(procode)
  ) 


providers_s2 <- bind_rows(
  providers_key_90,
  # providers_key_85
) |> 
  select(procode)

# 3. A1 ----------------------------------------------------------------------

## a. providers --------------------------------------------------------------

table_providers_s1 <- providers_s1 |>
  left_join(eric_2223, join_by(procode == trust_code)) |>
  select(1, 2) |>
  rename(Provider_Code = procode, Provider_Name = trust_name) |>
  arrange(Provider_Code) |>
  mutate(id = row_number(), .before = Provider_Code) |>
  mutate(Provider_Name = str_to_title(Provider_Name)) |>
  mutate(Provider_Name = str_replace_all(Provider_Name, "Nhs", "NHS")) 

## b. representation stats -----------------------------------------------

table_representation_trends <- tibble(
  data = list(
    dq_provider_represent_trend |>
      semi_join(providers_surviving, join_by(procode)),
    dq_provider_represent_trend |>
      semi_join(providers_s1, join_by(procode))
  )
) |>
  mutate(data_t3 = list(
    dq_provider_represent_trend_t3 |>
      semi_join(providers_surviving, join_by(procode)),
    dq_provider_represent_trend_t3 |>
      semi_join(providers_s1, join_by(procode))
  )) |> 
  mutate(n_providers = map_dbl(data, \(df)
                               df |>
                                 count(procode) |>
                                 nrow())) |>
  # THIS WILL BE SIZE IN 2023/24:
  # (FOR OTHER VARS WE'LL LOOK AT MEAN ACROSS 5 YR ECDS TIME PERIOD)
  mutate(size = map(data, \(df)
                    df |>
                      filter(fyear == "2023/24") |> 
                      count(fyear, procode, wt = n) |>
                      group_by(procode) |>
                      # MEAN OF A PROVIDER'S SIZE OVER 5 YEARS:
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
                        levels = c(
                          "attendances < 70k",
                          "attendances 70-94k",
                          "attendances 95-129k",
                          "attendances > 130k"
                          )
                      )) |>
                      count(size_group) |>
                      mutate(p = n / sum(n)) |>
                      select(-n) |>
                      pivot_wider(names_from = size_group, values_from = p)
                    
  )) |>
  
  mutate(arrive_ambulance = map_dbl(data, \(df)
                                    df |>
                                      count(fyear, is_ambulance, wt = n) |>
                                      group_by(fyear) |>
                                      mutate(p = n / sum(n)) |>
                                      ungroup() |>
                                      filter(is_ambulance == 1) |>
                                      summarise(av = round(mean(p), 3)) |>
                                      pull(av))) |>
  mutate(admitted = map_dbl(data, \(df)
                            df |>
                              count(fyear, is_adm, wt = n) |>
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
                           filter(fyear != "2023/24") |>
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
  unnest(region) |>
  unnest(size) |>
  unnest(four_hour) |>
  unnest(twelve_hour) |>
  select(-c(data, data_t3)) |>
  mutate(zz = if_else(n_providers > 50, "population", "sample"), .before = n_providers) |>
  pivot_longer(cols = !starts_with("zz"), names_to = "variable") |>
  pivot_wider(names_from = zz, values_from = value) |> 
  # table_representation_trends |> 
  select(variable, population, sample) |> 
  mutate(across(2:3, ~if_else(is.na(.), 0, .))) |>
  rowwise() |> 
  mutate(diff = sample - population) 


# 4. A2+ ------------------------------------------------------------------

## a. providers -----------------------------------------------------------

table_providers_s2 <- providers_s2 |> 
  left_join(eric_2223, join_by(procode == trust_code)) |> select(1,2) |> 
  rename(Provider_Code = procode, Provider_Name = trust_name) |> 
  arrange(Provider_Code) |> 
  mutate(id = row_number(), .before = Provider_Code) |> 
  mutate(Provider_Name = str_to_title(Provider_Name)) |> 
  mutate(Provider_Name = str_replace_all(Provider_Name, "Nhs", "NHS")) 


## b. representation stats -----------------------------------------------


table_representation_ecds_only <- tibble(
  data = list(
    dq_provider_represent_ecds_only |>
      semi_join(providers_surviving_ecds_only, join_by(procode)),
    dq_provider_represent_ecds_only |>
      semi_join(providers_s2, join_by(procode))
  )
) |>
  mutate(data_t3 = list(
    dq_provider_represent_ecds_only_t3 |>
      semi_join(providers_surviving_ecds_only, join_by(procode)),
    dq_provider_represent_ecds_only_t3 |>
      semi_join(providers_s2, join_by(procode))
  )) |> 
  mutate(n_providers = map_dbl(data, \(df)
                               df |>
                                 count(procode) |>
                                 nrow()
  )) |>
  mutate(f_year = map(data, \(df)
                      df |>
                        count(fyear, wt = n) |>
                        mutate(p = n / sum(n)) |>
                        filter(fyear == "2023/24") |>
                        mutate(fyear = "23/24 attendances (vs. 19/20)") |>
                        select(-n) |>
                        pivot_wider(names_from = fyear, values_from = p)
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
                           filter(fyear != "2023/24") |>
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
  unnest(f_year) |>
  unnest(size) |>
  unnest(region) |>
  unnest(four_hour) |>
  unnest(twelve_hour) |>
  # unnest(type) |>
  select(-c(data, data_t3)) |>
  mutate(zz = if_else(n_providers > 50, "population", "sample"), .before = n_providers) |>
  pivot_longer(cols = !starts_with("zz"), names_to = "variable") |>
  pivot_wider(names_from = zz, values_from = value) |> 
  select(variable, population, sample) |> 
  mutate(across(2:3, ~if_else(is.na(.), 0, .))) |> 
  rowwise() |> 
  mutate(diff = sample - population)

table_representation_ecds_only |> 
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
  fmt_percent(rows = c(2:26), decimals = 1) |>
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
  sub_values(values = -86, replacement = "") |> 
  gt_highlight_cols(sample, fill = "grey", alpha = 0.1) |> 
  tab_options(
    data_row.padding = px(1),
    table.font.size = "small"
  )

# EXPLORE WHERE CLAUSE IMPACT ------------------------------------------------------

# table_where_ecds_only <- tibble(
#   
#   data_where = list(
#     dq_provider_represent |>
#       filter(fyear %in% c("2019/20", "2023/24")) |> 
#       filter(na_arrival == 0) |>
#       filter(covid_exclusion == 0)  |>
#       semi_join(providers_surviving_ecds_only, join_by(procode)),
#     dq_provider_represent |>
#       filter(fyear %in% c("2019/20", "2023/24")) |> 
#       filter(na_arrival == 0) |>
#       filter(covid_exclusion == 0) |> 
#       semi_join(providers_s2, join_by(procode))
#   )
# ) 
# 
# dq_provider_represent |> 
#   select(starts_with("where")) |> 
#   colnames()
# 
# where_impact <- table_where_ecds_only |> 
#   mutate(data_where = map(data_where, \(df)
#                                df |>
#                                  filter(fyear == "2019/20")
#                                  # filter(fyear == "2023/24")
#   )) |> 
#   mutate(unfiltered = map_dbl(data_where, \(df)
#                                df |>
#                                  count(wt = n)|>
#                                  pull()
#   )) |> 
#   # mutate(r_prov_excl = map_dbl(data_where, \(df)
#   #                              df |>
#   #                                count(a = where_is_rprov == 1, wt = n)|>
#   #                                filter(a == 1) |>
#   #                                pull()
#   # )
#   # ) |>
#   # mutate(eng_excl = map_dbl(data_where, \(df)
#   #                              df |>
#   #                                count(a = where_is_rprov == 1 & where_is_eng == 1, wt = n)|>
#   #                                filter(a == 1) |>
#   #                                pull()
#   # )
#   # ) |>
#   # mutate(valid_excl = map_dbl(data_where, \(df)
#   #                              df |>
#   #                                count(a = where_is_rprov == 1 &
#   #                                        where_is_eng == 1 &
#   #                                        where_is_valid == 1, wt = n)|>
#   #                                filter(a == 1) |>
#   #                                pull()
#   # )
#   # ) |>
#   mutate(sex_etc_excl = map_dbl(data_where, \(df)
#                                df |>
#                                  count(a = where_is_rprov == 1 &
#                                          where_is_eng == 1 &
#                                          where_is_valid == 1 &
#                                          where_is_sex ==1 , wt = n)|>
#                                  filter(a == 1) |>
#                                  pull()
#   )
#   ) |>
#   mutate(type1_excl = map_dbl(data_where, \(df)
#                                df |>
#                                  count(a = where_is_rprov == 1 &
#                                          where_is_eng == 1 &
#                                          where_is_valid == 1 &
#                                          where_is_sex == 1 &
#                                          where_is_type1 == 1, wt = n)|>
#                                  filter(a == 1) |>
#                                  pull()
#   )
#   ) |>
#   mutate(attcat1_excl = map_dbl(data_where, \(df)
#                                df |>
#                                  count(a = where_is_rprov == 1 &
#                                          where_is_eng == 1 &
#                                          where_is_valid == 1 &
#                                          where_is_sex == 1 &
#                                          where_is_type1 == 1 &
#                                          where_is_attcat1 == 1 
#                                        , wt = n)|>
#                                  filter(a == 1) |>
#                                  pull()
#   )
#   ) |>
#   mutate(death_excl = map_dbl(data_where, \(df)
#                                df |>
#                                  count(a = where_is_rprov == 1 &
#                                          where_is_eng == 1 &
#                                          where_is_valid == 1 &
#                                          where_is_sex == 1 &
#                                          where_is_type1 == 1 &
#                                          where_is_attcat1 == 1 & 
#                                          where_is_live == 1  
#                                        , wt = n)|>
#                                  filter(a == 1) |>
#                                  pull()
#   )
#   ) |>
#   mutate(incompl_excl = map_dbl(data_where, \(df)
#                                df |>
#                                  count(a = where_is_rprov == 1 &
#                                          where_is_eng == 1 &
#                                          where_is_valid == 1 &
#                                          where_is_sex == 1 &
#                                          where_is_type1 == 1 &
#                                          where_is_attcat1 == 1 & 
#                                          where_is_live == 1 &
#                                          where_is_complete == 1
#                                        , wt = n)|>
#                                  filter(a == 1) |>
#                                  pull()
#   )
#   ) |>
#   identity()
# 
# where_impact |> 
#   mutate(across(3:7, ~ 1 - ./unfiltered))
# 
# # WHERE CLAUSE HAS LESS OF AN IMPACT FOR SAMPLE OF PROVIDERS THAN FOR POPULATION.
  
