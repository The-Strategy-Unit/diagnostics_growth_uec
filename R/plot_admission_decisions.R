# README
# Plot case-mix adjusted odds of test vs case-mix adjusted
# odds of admission

library("here")
library("dplyr")
library("tidyr")
library("tibble")
library("forcats")
library("janitor")
library("ggplot2")
library("ggrepel")
library("stringr")
library("scales")
library("lubridate")

df_odds_results <- bind_rows(
  # readRDS(here("data", "from_ncdr_growth_attrb_v3_1to16.rds"))
  readRDS(here("data", "from_ncdr_growth_attrb_v3_17to32.rds"))
) 

tmp_admodds <-
  readRDS(
  here("data", "from_ncdr_241106_admission_odds.rds")
  ) |> 
  filter(str_detect(term, "^fyear2023/24:")) |>
  # print(n=330)
  mutate(term = str_remove_all(term,"/")) |>
  mutate(term = str_remove(term,"fyear202324")) |> 
  mutate(term = str_remove_all(term,":")) |>
  mutate(term = str_sub(term, end = -2)) |> 
  select(term, odds_adm = odds)
  
# list.files(here("data_raw"))
  
lkp_invst_ec_aea <- readRDS(here("data_raw", "lkp_test_ec_to_aea.rds"))

# # NUMBER OF ATTENDANCES FROM ODDS MODEL DATA FRAME
# mutate(p = tests_2023_24 /0.566e6) |>
  
# -------------------------------------------------------------------------
df_odds_results |>
  # NUMBER OF ATTENDANCES FROM ODDS MODEL DATA FRAME:
  mutate(p = tests_2023_24 /0.566e6) |>
  left_join(tmp_admodds, join_by(name == term)) |>
  select(term = name, InvestigationDescription, odds, odds_adm, id, starts_with("tests"), p) |>
  mutate(InvestigationKey = as.numeric(str_extract(term, "[:digit:]{2}"))) |>
  left_join(
    lkp_invst_ec_aea |> 
      select(InvestigationKey, invst_group), 
    join_by(InvestigationKey)
  ) |> 
  # count(invst_group) |> 
  ggplot()+
  geom_hline(yintercept = 1, lty = "dashed", alpha = 0.1)+ # col = "grey40")+
  geom_vline(xintercept = 1, lty = "dashed", alpha = 0.1)+ # col = "grey40")+
  geom_point(aes(odds, odds_adm, size = p), alpha = 0.3, stroke = NA)+
  scale_size(labels = scales::percent_format())+
  geom_text_repel(
    aes(
      odds, odds_adm,
      label = str_wrap(InvestigationDescription, 20)
      ),
    size = 2,
  ) +
  facet_wrap(vars(invst_group))+
  theme_bw()+
  theme(
    axis.ticks = element_blank(),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 7),
    axis.title = element_text(size = 7)
  )+
  labs(
    x = "\nCase-mix-adjusted odds of test (2023/24 vs 2019/20)",
    y = "\nCase-mix-adjusted odds of admission having received test (2023/24 vs 2019/20)\n",
    size = "Attendances\nwith test\n2023/24"
  )

