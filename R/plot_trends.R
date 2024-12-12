# README
# Plot ED test rates for sample of 28 providers (NCDR DS SERVER)

library("tidytable")
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
  Server = "XXX",
  Database = "NHSE_Sandbox_StrategyUnit",
  Trusted_Connection = "True"
)

tb_trend <- tbl(con_sandbox_su, in_schema("dbo", "2232_diagnostics_trend"))


# 1. DATA MANIPULATION ------------------------------------------------------------

data_provider_sample <- tb_trend |>
  filter(procode %in% c(
    # SAMPLE OF PROVIDERS OBTAINED FROM DQ_TRENDS QUARTO DOC:
    "R1F",
    "R1H",
    "RAX",
    "RBL",
    "RDU",
    "REF",
    "RF4",
    "RHM",
    "RJ1",
    "RJ2",
    "RKB",
    "RLQ",
    "RM1",
    "RNS",
    "RP5",
    "RPA",
    "RQX",
    "RR8",
    "RRK",
    "RTD",
    "RTH",
    "RTX",
    "RWA",
    "RWY",
    "RX1",
    "RXF",
    "RXN",
    "RYR"
  )) |>
  count(fyear, procode, disdest_grp, Der_Investigation_All, wt = n) |>
  collect()

# USING AEA-STYLE CODES:
invst_codes <- c(paste0("0", 1:9), 10:23, 99)

list_invst_counts <- map(invst_codes, function(invst_code) {
  data_provider_sample %>%
    transmute(invst = str_count(Der_Investigation_All, str_c("^", invst_code, "| ", invst_code, "|,", invst_code)))
})

df_invst_rows <- list_invst_counts |>
  reduce(bind_cols) |>
  clean_names() |>
  rename(invst_99 = invst_24)

df_data_plus_invst <- data_provider_sample |>
  bind_cols(df_invst_rows) |>
  rename(n_att = n) |>
  # TIDYTABLE OPERATION:
  tidytable::mutate_rowwise(n_invst = sum(c_across(starts_with("invst_")), na.rm = T)) |>
  mutate(disdest = case_when(
    disdest_grp == "admitted" ~ "admitted",
    T ~ "non-admitted"
  ), .after = procode) |>
  mutate(total_invst = n_att * n_invst) |>
  select(-c(starts_with("invst_"), n_invst, Der_Investigation_All, disdest_grp))

###

df_preplot_trends <- df_data_plus_invst |>
  bind_rows(
    df_data_plus_invst |>
      group_by(fyear, procode) |>
      summarise(n_att = sum(n_att), total_invst = sum(total_invst)) |>
      ungroup() |>
      mutate(disdest = "overall (all destinations)", .after = procode)
  ) |>
  group_by(fyear, disdest) |>
  summarise(rate = sum(total_invst, na.rm = T) / sum(n_att, na.rm = T)) |>
  ungroup() |>
  mutate(disdest = case_when(
    disdest == "admitted" ~ "admitted patients",
    disdest == "non-admitted" ~ "non-admitted patients",
    T ~ disdest
  ))

# df_preplot_trends  |> saveRDS("from_ncdr_trends_240917_df_prep_plot.rds")
df_preplot_trends |> saveRDS("from_ncdr_trends_241003_df_preplot.rds")


# 2. PLOT -----------------------------------------------------------------

# FOR GRAPHIC FYEAR AXIS LABEL:
every_nth <- function(n) {
  return(function(x) {
    x[c(TRUE, rep(FALSE, n - 1))]
  })
}

# a. plot rate by destination --------------------------------------------------

df_preplot_trends |>
  ggplot(aes(fyear, rate)) +
  geom_line(aes(col = disdest, group = disdest)) +
  geom_point(aes(col = disdest, group = disdest)) +
  geom_blank(aes(y = 0)) +
  scale_color_manual(values = c("indianred3", "dodgerblue", "purple4")) +
  labs(x = "Financial year") +
  theme_bw() +
  labs(y = "Average number of recorded tests per ED attendance") +
  theme(
    legend.position = "none",
    axis.ticks = element_blank()
  ) +
  facet_wrap(vars(disdest)) +
  scale_x_discrete(breaks = every_nth(n = 4)) +
  scale_y_continuous(breaks = seq(0, 9, by = 2))

# b. plot rate by test type --------------------------------------------

preplot_trend_by_invest <- data_provider_sample |>
  bind_cols(df_invst_rows) |>
  rename(n_att = n) |>
  mutate(disdest = case_when(
    disdest_grp == "admitted" ~ "admitted",
    T ~ "non-admitted"
  ), .after = procode) |>
  mutate(across(starts_with("invst_"), ~ n_att * .)) |>
  group_by(fyear, procode, disdest) |>
  summarise(n_att = sum(n_att, na.rm = T), across(starts_with("invst_"), ~ sum(., na.rm = T))) |>
  ungroup() |>
  pivot_longer(cols = starts_with("invst_"), names_to = "invst_type", values_to = "n_invst") |>
  group_by(fyear, disdest, invst_type) |>
  reframe(n_att = sum(n_att, na.rm = T), n_invst = sum(n_invst, na.rm = T)) |>
  mutate(rate = n_invst / n_att)

preplot_trend_by_invest |> saveRDS("from_ncdr_trends_241003_preplot_by_invst.rds")

options(scipen = 999)
preplot_trend_by_invest |>
  ggplot(aes(fyear, rate)) +
  geom_line(aes(col = disdest, group = disdest)) +
  geom_point(aes(col = disdest, group = disdest)) +
  geom_blank(aes(y = 0)) +
  labs(x = "Financial year") +
  theme_bw() +
  labs(subtitle = "Vertical axis: Average number of investigations per A&E attendance") +
  theme(
    axis.title.y = element_blank()
  ) +
  facet_wrap(vars(invst_type), scales = "free_y") + # , scales = "free_y"
  scale_x_discrete(breaks = every_nth(n = 3)) +
  # scale_y_continuous(breaks = seq(0,9, by = 2))+
  NULL
