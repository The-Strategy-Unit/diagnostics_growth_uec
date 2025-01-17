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

# b. plot rate by test type --------------------------------------------

preplot_trend_by_invest <- readRDS(here("data_raw", "from_ncdr_trends_241003_preplot_by_invst.rds"))

preplot_trend_by_invest |>
  left_join(lkp_invst_ec_aea |> 
              mutate(
                # TODO NEED TO GET DESCRIPTIONS FOR LABELS
                
              )
            )
  
  
  ggplot(aes(fyear, rate)) +
  geom_line(aes(col = disdest, group = disdest)) +
  geom_point(aes(col = disdest, group = disdest)) +
  geom_blank(aes(y = 0)) +
  labs(
    x = "Financial year",
    y = "Average number of recorded tests per ED attendance",
    ) +
  theme_bw() +
  # labs(subtitle = "Vertical axis: Average number of recorded tests per ED attendance") +
  theme(
    axis.title.y = element_blank()
  ) +
  facet_wrap(vars(invst_type), scales = "free_y") + # , scales = "free_y"
  scale_x_discrete(breaks = every_nth(n = 3)) +
  # scale_y_continuous(breaks = seq(0,9, by = 2))+
  NULL
