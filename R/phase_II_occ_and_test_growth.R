# README
# [NCDR]
# Possible impact of results in context of ED diagnostic growth.

library("DBI")
library("here") 
library("dplyr")
library("purrr") 
library("furrr") 
library("readr") 
library("tidyr")
library("dbplyr")
library("tibble") 
library("forcats")
library("ggplot2") 
library("janitor")
library("stringr")
library("lubridate")


# 0. CONNECTIONS ----------------------------------------------------------

# con_sandbox_su <- dbConnect(
#   odbc::odbc(),
#   Driver = "SQL Server",
#   Server = "PRODNHSESQL101",
#   Database = "NHSE_Sandbox_StrategyUnit",
#   Trusted_Connection = "True"
# )

con_sus_plus <- dbConnect(
  odbc::odbc(),
  Driver = "SQL Server",
  Server = "PRODNHSESQL101",
  Database = "NHSE_SUSPlus_Live",
  Trusted_Connection = "True"
)

con_nhse_reference <- dbConnect(
  odbc::odbc(),
  Driver = "SQL Server",
  Server = "PRODNHSESQL101",
  Database = "NHSE_Reference",
  Trusted_Connection = "True"
)



# -------------------------------------------------------------------------

tb_ref_invst_ec <- tbl(con_nhse_reference, in_schema("dbo", "tbl_Ref_DataDic_ECDS_Investigation"))

lkp_invst <- tb_ref_invst_ec |>
  select(InvestigationKey, InvestigationCode, InvestigationDescription) |>
  # # REMOVE NO INVESTIGATION FROM SEARCH LIST
  # (NOTE: ITS PRESENCE DOESN'T INDICATE NO INVESTIGATION):
  filter(InvestigationKey != 98) |>
  collect() |> 
  mutate(InvestigationCode = as.character(InvestigationCode))


# -------------------------------------------------------------------------

query_1 <- 
  "
SELECT  
  Der_Financial_Year as fyear,
  Der_EC_Investigation_All AS invst_all,
  COUNT(*) AS n
  
  FROM NHSE_SUSPlus_Live.dbo.tbl_Data_SUS_EC ec 
    LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Arrival_Mode] ref_arr_mode ON ec.EC_Arrival_Mode_SNOMED_CT = ref_arr_mode.ArrivalModeCode
    -- LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Attendance_Source] ref_attsrc ON ec.EC_Attendance_Source_SNOMED_CT = ref_attsrc.AttendanceSourceCode
    -- LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Acuity] ref_acuity ON ec.EC_Acuity_SNOMED_CT = ref_acuity.AcuityCode
    -- LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Chief_Complaint] ref_chief_comp ON ec.EC_Chief_Complaint_SNOMED_CT = ref_chief_comp.ChiefComplaintCode
    -- LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Chief_Complaint_Group] ref_chief_comp_grp ON ec.EC_Chief_Complaint_SNOMED_CT = ref_chief_comp_grp.ChiefComplaintCode
    LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Discharge_Status] ref_dis_stat ON ec.EC_Discharge_Status_SNOMED_CT = ref_dis_stat.DischargeStatusCode
    LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Discharge_Destination] ref_dis_dest ON ec.Discharge_Destination_SNOMED_CT = ref_dis_dest.DischargeDestinationCode

WHERE 
-- IMPORTANT DISTINCTION IN THIS QUERY. REVMOVE MARCH FROM BOTH YEARS.
    MONTH(Arrival_Date) <> 3 

    AND ec.Der_Financial_Year IN (
        '2019/20',
        '2023/24'
    )
    AND EC_Department_Type IN ('01')
    AND -- ARRIVAL MODE KNOWN (AMBULANCE OR, IN ECDS, VARIOUS NAMED OTHERS) :
    (NOT ArrivalModeKey IS NULL)
    AND -- ATTENDANCE CATEGORY IS AN UNPLANNED FIRST (NOT FOLLOW UP / UNKNOWN):
    EC_AttendanceCategory = '1'
    AND -- NOT BROUGHT IN DEAD OR DIED DURING ATTENDANCE:
      (
      NOT (Der_AEA_Patient_Group = '70' OR Discharge_Destination_SNOMED_CT = '305398007') -- died
      )
    AND -- DURING ATTENDANCE DID NOT LEAVE / UNKNOWN DISPOSAL / NOT STREAMED PATIENTS (GENERALLY)
      (
      DischargeStatusDescription = 'Treatment completed (situation)'
      OR DischargeStatusDescription = 'Streamed to emergency department following initial assessment (situation)'
      )
    AND SEX IN ('1', '2')
    AND Age_At_Arrival IS NOT NULL 
    AND Der_Dupe_Flag = 0
    AND LEFT(Der_Postcode_Dist_Unitary_Auth, 1) = 'E'
    AND LEFT(Der_Provider_Code, 1) = 'R'
    
  GROUP BY 
  Der_Financial_Year,
  Der_EC_Investigation_All
"

tmp <- dbGetQuery(con_sus_plus, query_1) |> as_tibble()

tmp <- tmp |> as_tibble()

# PULL EC TYPE CODES:
vec_invst_codes_ec <- lkp_invst |>
  mutate(InvestigationCode = as.character(InvestigationCode)) |>
  mutate(InvestigationKey = str_c("invst_", InvestigationKey)) |>
  select(InvestigationKey, InvestigationCode) |>
  deframe()


# NOTE: COUNTING INSTANCES HERE TO KEEP OPTIONS OPEN.
# BUT LATER ADJUSTED TO BINARY (TEST Y/N).
list_invst_counts_all <- imap(vec_invst_codes_ec, function(x, y) {
  tmp %>%
    transmute({{ y }} := str_count(invst_all, str_c("^", x, "| ", x, "|,", x)))
})

gc()

df_invst_all <- list_invst_counts_all |>
  reduce(bind_cols) |>
  bind_cols(tmp, y = _)

gc()

df_invst_all <- df_invst_all |>
  # head(10) |>
  select(-invst_all) |> 
  rowwise() |> 
  mutate(across(starts_with("invst_"), ~ n * .), .after = fyear) |> 
  ungroup() 


df_fyear_counts <- df_invst_all |>
  # head(10) |>
  group_by(fyear) |>
  summarise(
    across(
      starts_with("invst_"),
      ~ sum(., na.rm = T)
    )
  ) |>
  ungroup() |>
  pivot_longer(cols = starts_with("invst"), names_to = "test", values_to = "n") |>
  # arrange(-n) |> 
  pivot_wider(names_from = fyear, values_from = n) |> 
  clean_names() |> 
  mutate(growth_abs = x2023_24 -x2019_20)

df_fyear_counts |> 
  mutate(InvestigationKey = as.numeric(str_extract(test, "[:digit:]{2}"))) |>
  left_join(
    lkp_invst |>
      select(InvestigationKey, InvestigationDescription),
    join_by(InvestigationKey)
  ) |>
  relocate(InvestigationDescription, 1) |> 
  select(-c(InvestigationKey)) |> 
  saveRDS("df_phase_II_fyear_growth_counts.rds")


# left_join(
#   # df_counts |>
#   #   count(fyear, name = "n_att"),
#   # join_by(fyear)
# ) |>
# rename(n_tests = n)

df_counts |>
  arrange(fyear) |>
  pivot_wider(names_from = fyear, values_from = c(n_att, n_tests)) |>
  clean_names() |>
  mutate(test_growth = n_tests_2023_24 / n_tests_2019_20) |>
  mutate(test_rate_1920 = n_tests_2019_20 / n_att_2019_20) |>
  mutate(test_rate_2324 = n_tests_2023_24 / n_att_2023_24) |>
  mutate(rate_growth = test_rate_2324 / test_rate_1920) |>
  arrange(-rate_growth) |>
  select(-starts_with("test_r")) |>
  mutate(InvestigationKey = as.numeric(str_extract(test, "[:digit:]{2}"))) |>
  left_join(
    lkp_invst |>
      select(InvestigationKey, InvestigationDescription),
    join_by(InvestigationKey)
  ) |>
  relocate(InvestigationDescription, 1) |>
  saveRDS(here("data", "df_growth_counts.rds"))

