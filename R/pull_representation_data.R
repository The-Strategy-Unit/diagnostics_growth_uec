# README
# Pulls representation table from server and saves. 
# RDS object used in final report. 

library("DBI")
library("dplyr")
library("dbplyr")
library("janitor")

con_sandbox_su <- dbConnect(
  odbc::odbc(),
  Driver = "SQL Server",
  Server = 
    Database = 
    Trusted_Connection = "True"
)

tbl(con_sandbox_su, in_schema("dbo", "2232_diagnostics_provider_representation_stats")) |> 
  collect() |> 
  clean_names() |> 
  # saveRDS("from_ncdr_240917_prov_represent.rds")
  saveRDS("from_ncdr_250116_prov_represent.rds")
  
