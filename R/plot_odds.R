# README
# Plot results of "odds of test" model.
# TODO CONFIDENCE INTERVALS?

library("here")
library("dplyr")
library("tidyr")
library("tibble")
library("forcats")
library("janitor")
library("ggplot2")
library("ggrepel")
library("stringr")
library("lubridate")


df_odds_results <- bind_rows(
  readRDS(here("data", "from_ncdr_growth_attrb_v3_1to16.rds")),
  # readRDS(here("data", "from_ncdr_growth_attrb_v3_17to32.rds"))
)

# 1. SIMPLE BARS ----------------------------------------------------------

df_odds_results |>
  ggplot() +
  geom_hline(yintercept = 1, lty = "dashed", colour = "grey20") +
  geom_hline(yintercept = 2, lty = "dashed", colour = "grey20") +
  geom_col(aes(reorder(InvestigationDescription, odds), odds), alpha = .8) +
  theme_bw() +
  theme_minimal() +
  coord_flip() +
  scale_y_continuous(
    breaks = seq(0, 2.5, by = 0.5),
    labels = c(
      "",
      "0.5x\nas likely in\n2023/24",
      "1x\nas likely in\n2023/24",
      "1.5x\nas likely in\n2023/24",
      "2x\nas likely in\n2023/24",
      ""
    ),
    position = "right"
  ) +
  theme(
    axis.title.y = element_blank()
  ) +
  labs(y = "Case-mix-adjusted odds ratio of test (2023/24 vs 2019/20)\n")


# 2. SCATTER (WITH VOLUMES) ----------------------------------------------

df_odds_results |>
  # DENOM IS THE NUMBER OF 2023/24 ATTENDANCES IN NESTED DFS FOR ODDS MODEL:
  mutate(p = tests_2023_24 / 0.566e6) |>
  ggplot(aes(odds, p)) +
  theme_bw() +
  theme_minimal() +
  geom_point() +
  geom_blank(aes(x = 0, y = 0)) +
  geom_text_repel(
    aes(label = str_wrap(InvestigationDescription, 20)),
    size = 2,
  ) +
  scale_y_continuous(
    limits = c(0, 0.5),
    labels = scales::percent_format()
  ) +
  scale_x_continuous(
    limits = c(0, 2.5),
    breaks = seq(0, 2.5, by = 0.5),
    labels = c(
      "",
      "0.5x\nas likely in\n2023/24",
      "1x\nas likely in\n2023/24",
      "1.5x\nas likely in\n2023/24",
      "2x\nas likely in\n2023/24",
      ""
    ),
    position = "top"
  ) +
  labs(
    x = "Case-mix-adjusted odds ratio of test (2023/24 vs 2019/20)\n",
    y = "Proportion of attendances involving test (2023/24)\n"
  )
