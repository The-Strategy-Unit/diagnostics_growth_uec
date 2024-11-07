# README
# Plot growth in testing by components of demand, case-mix, and practice. 

library("here")
library("dplyr")
library("tidyr")
library("tibble")
library("janitor")
library("ggplot2")
library("ggrepel")
library("stringr")
library("lubridate")


# 1. LOAD DATA AND PREP ---------------------------------------------------

df_odds_results <- bind_rows(
  readRDS(here("data", "from_ncdr_growth_attrb_v3_1to16.rds")),
  # readRDS(here("data", "from_ncdr_growth_attrb_v3_17to32.rds"))
)

df_preplot_growth_components <- df_odds_results |>
  mutate(d0 = 1, .before = dT) |>
  relocate(dC, .before = dP) |>
  pivot_longer(cols = matches("^d|^abs"), names_to = "component", values_to = "value") |>
  select(-tests_2023_24) |>
  mutate(value_abs = tests_2019_20 * value) |>
  mutate(value_order = if_else(component == "d0", value_abs, NA_real_)) |>
  mutate(value = if_else(component == "d0", NA_real_, value)) |>
  mutate(component = case_when(
    component == "d0" ~ "Attds with test\n19/20",
    component == "dT" ~ "Growth in attds with test\n19/20 to 23/24",
    component == "dD" ~ "Growth driven by demand",
    component == "dC" ~ "Growth driven by case-mix",
    component == "dP" ~ "Growth driven by practice",
    component == "dI" ~ "Growth due interactions",
    T ~ NA_character_
  )) |>
  mutate(component = forcats::as_factor(component)) |>
  mutate(value_abs = as.integer(value_abs)) |>
  group_by(InvestigationDescription) |>
  mutate(value_order = max(value_order, na.rm = T)) |>
  ungroup() |>
  mutate(value_abs = round(value_abs / 1000)) |>
  mutate(value_label_abs = if_else(is.na(value), str_c(value_abs, "k"), NA_character_)) |>
  mutate(value_label_p = if_else(value >= 0, str_c("+", round(value * 100), "%"), str_c(round(value * 100), "%")))


# 2. PLOT -------------------------------------------------------------------

## a. abs changes ---------------------------------------------------------------

df_preplot_growth_components |>
  mutate(value_label_abs = str_c(value_abs, "k")) |>
  ggplot(aes(reorder(InvestigationDescription, value_order), value_abs)) +
  geom_col(
    aes(reorder(InvestigationDescription, value_order), value_abs, fill = component),
    position = "identity",
    alpha = 0.4
  ) +
  geom_text(
    aes(
      label = value_label_abs,
      vjust = ifelse(value_abs >= 0, -0.4, 1.4),
      angle = 270
    ),
    size = 1.8,
    colour = "grey20"
  ) +
  coord_flip() +
  geom_hline(yintercept = 0, lty = "dashed") +
  theme_minimal() +
  theme(
    axis.ticks = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none",
    strip.text = element_text(size = 7),
    axis.text = element_text(size = 5),
    panel.grid.minor = element_blank(),
    # panel.spacing = unit(1, "lines")
  ) +
  scale_y_continuous(
    limits = c(-50, 250)
  ) +
  facet_wrap(vars(component), ncol = 6) +
  labs(y = "\nED attendances (thousands)\nwith 1+ test of specified type")


## b. % changes ---------------------------------------------------------------

df_preplot_growth_components |>
  ggplot(aes(reorder(InvestigationDescription, value_order), value_abs)) +
  geom_col(
    aes(
      reorder(InvestigationDescription, value_order), value_abs,
      fill = component
    ),
    position = "identity",
    alpha = 0.4
  ) +
  geom_text(
    aes(
      label = value_label_abs,
      vjust = ifelse(value_abs >= 0, -0.4, 1.4),
      angle = 270
    ),
    size = 1.5,
    colour = "grey20"
  ) +
  geom_text(
    aes(
      label = value_label_p,
      vjust = ifelse(value_abs >= 0, -0.4, 1.4),
      angle = 270
    ),
    size = 1.5,
    colour = "grey20"
  ) +
  coord_flip() +
  geom_hline(yintercept = 0, lty = "dashed") +
  theme_minimal() +
  theme(
    axis.ticks = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none",
    strip.text = element_text(size = 7),
    axis.text = element_text(size = 5),
    panel.grid.minor = element_blank(),
    # panel.spacing = unit(1, "lines")
  ) +
  scale_y_continuous(
    limits = c(-50, 250)
  ) +
  facet_wrap(vars(component), ncol = 6) +
  labs(y = "\nED attendances (thousands)\nwith 1+ test of specified type")

