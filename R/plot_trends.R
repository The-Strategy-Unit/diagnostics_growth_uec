# README
# Plot ED test rates for sample of 28 providers 

library("here")
library("dplyr")
library("purrr")
library("tidyr")
library("forcats")
library("ggplot2")
library("stringr")
library("janitor")
library("lubridate")

lkp_invst_ec_aea <- readRDS(here("reference", "lkp_test_ec_to_aea.rds"))

df_preplot_trend <- readRDS(
  here("data", "from_ncdr_trends_241204_df_preplot.rds")
) |> 
  mutate(disdest = fct_relevel(disdest, "Overall (all destinations)")) |> 
  mutate(disdest = fct_relevel(disdest, "Non-admitted patients", after = 1))

# NULL LOGIC:
# df_preplot_trends <- readRDS(here("data", "from_ncdr_trends_250115_df_preplot.rds")) |>
#   mutate(disdest = fct_relevel(disdest, "Overall (all destinations)")) |>
#   mutate(disdest = fct_relevel(disdest, "Non-admitted patients", after = 1))

# FOR GRAPHIC FYEAR AXIS LABEL:
every_nth <- function(n) {
  return(function(x) {
    x[c(TRUE, rep(FALSE, n - 1))]
  })
}

# 1. PLOT BY DESTINATION -----------------------------------------------------------------

# df_preplot_trends |>
#   ggplot(aes(fyear, rate)) +
#   geom_line(aes(col = disdest, group = disdest)) +
#   geom_point(aes(col = disdest, group = disdest)) +
#   geom_blank(aes(y = 0)) +
#   scale_color_manual(values =  c("#8F2D56", "dodgerblue", "#D81159")) +
#   labs(x = "Financial year") +
#   theme_bw() +
#   labs(y = "Average number of recorded tests per ED attendance\n") +
#   theme(
#     legend.position = "none",
#     axis.ticks = element_blank()
#   ) +
#   facet_wrap(vars(disdest)) +
#   scale_x_discrete(breaks = every_nth(n = 4)) +
#   scale_y_continuous(breaks = seq(0, 9, by = 2))

df_preplot_trend |>
  ggplot(aes(fyear, rate)) +
  geom_line(aes(col = disdest, group = disdest)) +
  geom_point(aes(col = disdest, group = disdest)) +
  geom_blank(aes(y = 0)) +
  scale_color_manual(values = c("#8F2D56", "dodgerblue", "#D81159")) +
  labs(x = "Financial year")+
  theme_bw()+
  labs(y = "Average number of recorded tests per ED attendance\n")+
  theme(
    legend.position = "none",
    axis.ticks = element_blank()
  )+
  facet_wrap(vars(disdest))+ # , scales = "free_y"
  scale_x_discrete(breaks = every_nth(n=4))+
  scale_y_continuous(breaks = seq(0,9, by = 2))

# b. plot rate by test group --------------------------------------------

readRDS(here("data_raw", "from_ncdr_trends_241003_preplot_by_invst.rds")) |>
  left_join(
    readRDS(here("reference", "lkp_test_ec_to_aea.rds")) |> 
      distinct(Der_AEA_Investigation_All, AEA_Investigation_Desc_Short, invst_group) |> 
      mutate(invst_type = str_c("invst_", str_remove(Der_AEA_Investigation_All, "^0"))),
    join_by(invst_type)
              ) |> 
  filter(!str_detect(AEA_Investigation_Desc_Short, "ental")) |> 
  filter(!is.na(AEA_Investigation_Desc_Short)) |>
  mutate(AEA_Investigation_Desc_Short = as_factor(AEA_Investigation_Desc_Short)) |>
  mutate(AEA_Investigation_Desc_Short = fct_reorder(AEA_Investigation_Desc_Short, as.integer(invst_group))) |> 
  ###
  group_by(fyear, disdest, invst_group, n_att) |> 
  summarise(n_invst = sum(n_invst)) |> 
  ungroup() |> 
  # View("tablow")
  mutate(rate = n_invst / n_att) |> 
  
  ggplot(aes(fyear, rate*1000)) +
  geom_line(aes(col = disdest, group = disdest)) +
  geom_point(aes(col = disdest, group = disdest)) +
  # geom_smooth(aes(col = disdest, group = disdest), se = F) +
  geom_blank(aes(y = 0)) +
  labs(
    x = "Financial year",
    y = "Average number of recorded tests per thousand ED attendance",
    ) +
  theme_bw() +
  scale_color_manual(values = c("#D81159", "dodgerblue")) +
  theme(
    # axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(size = 5)
  ) +
  facet_wrap(vars(invst_group)) + # , scales = "free_y"
  scale_x_discrete(breaks = every_nth(n = 3)) +
  guides(colour = guide_legend(title = "Patient outcome"))+
# facet_grid(invst_group ~ AEA_Investigation_Desc_Short, space = "free", scales = "free_y")
  # scale_y_continuous(breaks = seq(0,9, by = 2))+
  NULL

# c. plot rate by test type --------------------------------------------

readRDS(here("data_raw", "from_ncdr_trends_241003_preplot_by_invst.rds")) |>
  left_join(
    readRDS(here("reference", "lkp_test_ec_to_aea.rds")) |> 
      distinct(Der_AEA_Investigation_All, AEA_Investigation_Desc_Short, invst_group) |> 
      mutate(invst_type = str_c("invst_", str_remove(Der_AEA_Investigation_All, "^0"))),
    join_by(invst_type)
              ) |> 
  filter(!str_detect(AEA_Investigation_Desc_Short, "ental")) |> 
  filter(!is.na(AEA_Investigation_Desc_Short)) |>
  mutate(AEA_Investigation_Desc_Short = as_factor(AEA_Investigation_Desc_Short)) |>
  mutate(AEA_Investigation_Desc_Short = fct_reorder(AEA_Investigation_Desc_Short, as.integer(invst_group))) |> 
  ###
  
  ggplot(aes(fyear, rate*1000)) +
  # geom_line(aes(col = disdest, group = disdest)) +
  geom_point(aes(col = disdest, group = disdest), size = 1) +
  geom_smooth(aes(col = disdest, group = disdest), se = F, linewidth = .5) +
  geom_blank(aes(y = 0)) +
  labs(
    x = "Financial year",
    y = "Average number of recorded tests per ED attendance",
    ) +
  theme_bw() +
  scale_color_manual(values = c("#D81159", "dodgerblue")) +
  theme(
    # axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(size = 5)
  ) +
  facet_wrap(vars(AEA_Investigation_Desc_Short), scales = "free_y") + # 
  scale_x_discrete(breaks = every_nth(n = 3)) +
  guides(colour = guide_legend(title = "Patient outcome"))+
# facet_grid(invst_group ~ AEA_Investigation_Desc_Short, space = "free", scales = "free_y")
  # scale_y_continuous(breaks = seq(0,9, by = 2))+
  NULL
