# README
# Plots to highlight the impact of multiple tests on time in ED.
# Here we ignore casemix and examine only the un-adjusted impact.

library("here")
library("dplyr")
library("purrr")
library("tidyr")
library("scales")
library("tibble")
library("forcats")
library("janitor")
library("ggplot2")
library("stringr")
library("lubridate")

# PLOT -----------------------------

## a. attd overall: all attendances - number of tests (full distribution) --------
readRDS(here("data", "df_nested_preplots_multi_tests.rds"))$data[[1]] |>
  ggplot() +
  theme_bw() +
  # theme_minimal() +
  geom_vline(aes(xintercept = mean_tests, colour = fyear), linetype = "dashed") +
  geom_line(aes(x = n_tests_all, y = p_patients, colour = fyear)) +
  geom_point(aes(x = n_tests_all, y = p_patients, colour = fyear)) +
  scale_y_continuous(
    name = "Proportion of attendances",
    label = label_percent(accuracy = 1)
  ) +
  scale_x_continuous(name = "number of tests") +
  labs(
    title = "Number of tests per attendance",
    subtitle = "Selected providers | Apr19-Feb20 & Apr23-Feb24",
    caption = "dashed lines indicate mean number of tests"
  )

# TODO Y AXIS
## b. attd number of tests grouped --------

readRDS(here("data", "df_nested_preplots_multi_tests.rds"))$data[[2]] |>
  ggplot() +
  geom_col(
    aes(fyear, p_patients_adj, fill = n_tests_all_grp),
    position = position_stack()
  ) +
  geom_hline(aes(yintercept = 0), colour = "grey20", lty = "dashed") +
  # geom_col(
  #   aes(fyear, rev(p_patients), colour = n_tests_all_grp),
  #   fill = "transparent",
  #   colour = "grey20",
  #   lty = "dashed",
  #   position = position_stack()
  # ) +
  geom_text(aes(x = fyear, y = p_patients_adj, group = n_tests_all_grp, label = p_patients_label),
    position = position_stack(vjust = 0.5)
  ) +
  scale_fill_manual(values = c("red", "orange", "yellow", "grey")) +
  # scale_colour_manual(values = c("red", "orange", "yellow", "grey")) +
  scale_y_continuous(
    name = "Proportion of attendances",
    label = label_percent(accuracy = 1)
  ) +
  scale_x_discrete(name = "Financial year") +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    panel.grid = element_blank(),
    axis.text.y = element_blank()
  ) +
  labs(
    title = "Number of tests per attendance",
    subtitle = "Selected providers | Apr19-Feb20 & Apr23-Feb24"
  )

## c. attd split by admitted/non-admitted and injury/illness -------

readRDS(here("data", "df_nested_preplots_multi_tests.rds"))$data[[3]] |>
  ggplot() +
  geom_col(aes(x = fyear, y = p_patients_adj, fill = n_tests_all_grp),
    position = position_stack()
  ) +
  geom_hline(aes(yintercept = 0), colour = "grey20", lty = "dashed") +
  geom_text(
    aes(x = fyear, y = p_patients_adj, group = n_tests_all_grp, label = p_patients_label),
    position = position_stack(vjust = 0.5),
    size = 2
  ) +
  facet_grid(cols = vars(is_admitted), rows = vars(inj_illness)) +
  scale_fill_manual(values = c("red", "orange", "yellow", "grey")) +
  scale_y_continuous(
    name = "Proportion of attendances",
    label = label_percent(accuracy = 1)
  ) +
  scale_x_discrete(name = "Financial year") +
  theme(legend.title = element_blank()) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  labs(
    title = "Number of tests per patient by disposal and presentation type",
    subtitle = "Selected providers | Apr19-Feb20 & Apr23-Feb24"
  )

## d. attd split by test type------

readRDS(here("data", "df_nested_preplots_multi_tests.rds"))$data[[4]] |>
  ggplot() +
  geom_col(aes(x = fyear, y = p_patients_adj, fill = n_tests_grp),
    position = position_stack()
  ) +
  geom_hline(aes(yintercept = 0), colour = "grey20", lty = "dashed") +
  geom_text(aes(x = fyear, y = p_patients_adj, group = n_tests_grp, label = p_patients_label),
    position = position_stack(vjust = 0.5)
  ) +
  facet_wrap(vars(test_type)) +
  scale_fill_manual(values = c("orange", "yellow", "grey")) +
  scale_y_continuous(
    name = "Proportion of attendances",
    # label = label_percent(accuracy = 1),
    breaks = c(-0.5, -0.25, 0, 0.25, 0.5),
    labels = c("50%", "25%", "0%", "25%", "50%")
  ) +
  scale_x_discrete(name = "Financial year") +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  labs(
    title = "Number of tests per patient by test type",
    subtitle = "Selected providers | Apr19-Feb20 & Apr23-Feb24"
  )

## e. time overall ----------------

readRDS(here("data", "df_nested_preplots_multi_tests.rds"))$data[[5]] |>
  ggplot() +
  theme_minimal() +
  theme(
    axis.ticks = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  geom_line(aes(
    x = n_tests_trunc,
    y = median_dur_assess_depart,
    colour = fyear
  )) +
  geom_point(aes(
    x = n_tests_trunc,
    y = median_dur_assess_depart,
    colour = fyear,
    size = n_patients
  )) +
  scale_y_continuous(
    name = "Median time in ED (assessment to departure)",
    limits = c(0, NA_real_)
  ) +
  scale_x_continuous(
    name = "Number of tests",
    limits = c(0, 12),
    breaks = (0:12),
    labels = c(
      "0", "1", "2", "3", "4",
      "5", "6", "7", "8", "9",
      "10", "11", "12+"
    )
  ) +
  scale_size_continuous(name = "Number of attendances") +
  scale_colour_discrete(name = "Financial year") +
  labs(
    title = "Median time in ED by number of tests",
    subtitle = "Selected providers | Apr19-Feb20 & Apr23-Feb24"
  )


## f. time split by admitted/non-admitted and injury/illness -----
readRDS(here("data", "df_nested_preplots_multi_tests.rds"))$data[[6]] |>
  ggplot() +
  # theme_minimal()+
  theme_bw() +
  theme(
    axis.ticks = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  geom_line(aes(
    x = n_tests_trunc,
    y = median_dur_assess_depart,
    colour = fyear
  )) +
  geom_point(aes(
    x = n_tests_trunc,
    y = median_dur_assess_depart,
    colour = fyear,
    size = n_patients
  )) +
  facet_grid(
    cols = vars(is_admitted),
    rows = vars(inj_illness)
  ) +
  scale_y_continuous(
    name = "Median time in ED (assessment to departure)",
    limits = c(0, NA_real_)
  ) +
  scale_x_continuous(
    name = "Number of tests",
    limits = c(0, 12),
    breaks = (0:12),
    labels = c(
      "0", "1", "2", "3", "4",
      "5", "6", "7", "8", "9",
      "10", "11", "12+"
    )
  ) +
  scale_size_continuous(name = "Number of attendances") +
  scale_colour_discrete(name = "Financial year") +
  labs(
    title = "Median time in ED and number of tests by dispsosal and presentation type",
    subtitle = "Selected providers | Apr19-Feb20 & Apr23-Feb24"
  )


## g. time split by test type -----
readRDS(here("data", "df_nested_preplots_multi_tests.rds"))$data[[7]] |>
  ggplot() +
  theme_bw() +
  theme(
    axis.ticks = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  geom_line(aes(
    x = n_tests_trunc,
    y = median_dur_assess_depart,
    colour = fyear
  )) +
  geom_point(aes(
    x = n_tests_trunc,
    y = median_dur_assess_depart,
    colour = fyear,
    size = n_patients
  )) +
  facet_wrap(vars(test_type)) +
  scale_y_continuous(
    name = "Median time in ED (assessment to departure)",
    limits = c(0, NA_real_)
  ) +
  scale_x_continuous(
    name = "Number of tests",
    limits = c(0, 5),
    breaks = (0:5),
    labels = c(
      "0", "1", "2", "3", "4",
      "5+"
    )
  ) +
  scale_size_continuous(name = "Number of attendances") +
  scale_colour_discrete(name = "Financial year") +
  labs(
    title = "Median time and number of tests in ED by test type",
    subtitle = "Selected providers | Apr19-Feb20 & Apr23-Feb24"
  )
