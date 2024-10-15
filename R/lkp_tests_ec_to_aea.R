# README
# Lookups providing names of EC investigations and a mapping of EC to AEA codes.
# A "test group" - from previous work - has also been added.

library("DBI")
library("dplyr")
library("tidyr")
library("dbplyr")

source("create_study_dataset.R")


# 1. DB CONNECTION --------------------------------------------------------

con_nhse_reference <- dbConnect(
  odbc::odbc(),
  Driver = "SQL Server",
  Server = "PRODNHSESQL101",
  Database = "NHSE_Reference",
  Trusted_Connection = "True"
)

tb_ref_invst_ec <- tbl(con_nhse_reference, in_schema("dbo", "tbl_Ref_DataDic_ECDS_Investigation"))
tb_ref_invst_aea <- tbl(con_nhse_reference, in_schema("dbo", "tbl_Ref_DataDic_AEA_Investigation"))


# 2. LOOKUP EC ------------------------------------------------------------

lkp_invst <- tb_ref_invst_ec |>
  select(InvestigationKey, InvestigationCode, InvestigationDescription) |>
  # # REMOVE NO INVESTIGATION FROM SEARCH LIST
  # (NOTE: ITS PRESENCE DOESN'T INDICATE NO INVESTIGATION):
  filter(InvestigationKey != 98) |>
  collect() |> 
  mutate(InvestigationCode = as.character(InvestigationCode))

lkp_invst_aea <- tb_ref_invst_aea |> 
  select(-AEA_Investigation_Desc) |> 
  collect() |> 
  mutate(AEA_Investigation_Desc_Short = str_remove(AEA_Investigation_Desc_Short, "[:digit:]{2}: "))

# 3. LOOKUP EC to AEA ------------------------------------------------

lkp_tests_ec_to_aea <- pared_provider_sample |> 
  select(Der_EC_Investigation_All, Der_AEA_Investigation_All) |> 
  filter(str_length(Der_AEA_Investigation_All) == 2) |> 
  distinct(Der_AEA_Investigation_All, Der_EC_Investigation_All) |> 
  add_row(Der_AEA_Investigation_All = "03", Der_EC_Investigation_All = "56027003") |> 
  arrange(Der_AEA_Investigation_All) |> 
  left_join(lkp_invst_aea, join_by(Der_AEA_Investigation_All == AEA_Investigation)) |> 
  left_join(lkp_invst, join_by(Der_EC_Investigation_All == InvestigationCode)) |> 
  relocate(InvestigationKey, .after = Der_AEA_Investigation_All) |> 
  # FROM FIG. 4.1 OF "Waiting Times and Attendance Durations at English A&E Departments" REPORT:
  mutate(invst_group = case_when(
    Der_AEA_Investigation_All %in% c("01", "10", "11", "12", "13", "22") ~ "Imaging",
    Der_AEA_Investigation_All %in% c("03", "04", "14") ~ "Haematology",
    Der_AEA_Investigation_All %in% c("05", "16", "17", "18", "21") ~ "Biochemistry",
    Der_AEA_Investigation_All %in% c("02", "06", "07", "08", "15", "19", "20", "23", "99") ~ "Other",
    T ~ NA_character_
  )) 
