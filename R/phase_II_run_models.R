# README
# [NCDR]
# Run models used in Phase II.

library("here") 
library("dplyr")
library("purrr") 
library("furrr") 
library("readr") 
library("tidyr")
library("tibble") 
library("forcats")
library("ggplot2") 
library("janitor")
library("stringr")
library("lubridate")

# 1. FUNCTIONAL PROGRAMMING SETUP (FOR MULTIPLE MODELS) ---------------------
# FOUR MODELS RUN IN PARALLEL USING FURRR PACKAGE

# CREATE DATA FRAME FOR FUNCTIONAL PROGRAMMING:
df1 <- df_prep_binary |>
  summarise(across(starts_with("invst_"), ~ sum(.))) |>
  pivot_longer(cols = everything()) |>
  arrange(-value) |>
  mutate(id = row_number()) |> 
  # X to Y (OF TOP Z) BY APPEARANCES:
  # slice(1:4) |>
  slice(5:12) |>
  mutate(InvestigationKey = as.numeric(str_extract(name, "[:digit:]{2}"))) |>
  left_join(lkp_invst, join_by(InvestigationKey)) |>
  select(name, value, InvestigationDescription, id) |> 
  # REMOVE SUPERFLUOUS TEXT:
  mutate(InvestigationDescription = str_remove_all(InvestigationDescription, "[:punct:]")) |>
  mutate(InvestigationDescription = str_remove_all(InvestigationDescription, " procedure| situation")) |> 
  relocate(InvestigationDescription, .after = name) |> 
  print(n=35)

# NOTE!!
# IF YOU NEED TO PREVIEW df2 (BELOW) BEST DONE IN CONSOLE WINDOW
# RATHER THAN WITH view()

# ADD REQUIRED DATA IN LIST COLUMN (AND MANIPULATE):
df2 <- df1 |>
  mutate(data = list(df_prep_binary)) |>
  mutate(data = map2(data, name, function(df, x) {
    df |>
      select(everything(), -matches("invst"), all_of(x)) |>
      mutate(across(all_of(x), ~ as.factor(.))) |>
      # TO AVOID COMPLICATION IN MODEL FORMULA:
      rename("invst" = x)
  }))

gc()
gc()

# PREVIEW:
# df2
# -------------------------------------------------------------------------

plan(multisession, workers = 8)
future::futureSessionInfo()

tictoc::tic()
df3 <- df2 |>
  mutate(model = future_map(data, function(df) {
    mgcv::gam(
      formula = invst ~
        # # VAR OF INTEREST:
        occ_scaled +
        # occ_decile +
        # occ_quintile +
        # DEMOGRAPHICS:
        s(age, by = sex) + sex + # imd_dec +
        # CASE-MIX-RELATED:
        arr_mode + acuity + chief_comp_desc + refer_sorc +
        # TIME-RELATED:
        is_winter + is_wkend + is_night +
        # PROVIDER (RANDOM INTERCEPT):
        s(procode, bs = "re"),
      family = "binomial",
      method = "REML",
      data = df
    )
  })) 
tictoc::toc()
# <25 MINS FOR 250K RECORDS, 4 MODELS IN PARALLEL
# < 1.5 HOURS FOR 700K RECORDS, 4 MODELS IN PARALLEL
# < 2 HOURS FOR 1M RECORDS, 4 MODELS IN PARALLEL
# 4925.32 /60

gc()

df
df3$model[[1]] |> summary()

df3$model[[3]] |> 
  broom::tidy(parametric = TRUE) |> 
  filter(str_detect(term, "quint")) |>
  mutate(odds = exp(estimate)) |>
  mutate(lci = exp(estimate - 1.96*std.error)) |> 
  mutate(uci = exp(estimate + 1.96*std.error)) |> 
  select(term, odds, lci, uci)

24*25*10
4293/60
# 9. SAVE MODEL RESULTS -------------------------------------------------------------
# list.files(pattern = "200k")
df3 %>% 
  select(-data) %>%
  saveRDS(str_c("models_odds_occ_", min(.$id), "to", max(.$id), ".rds"))

df3 <- read_rds(str_c("models_odds_occ_", 1, "to", 4, ".rds"))
df3a <- read_rds(str_c("models_odds_occ_", 5, "to", 16, ".rds"))

df_odds_results <- df3 |>
  mutate(results = map(model, function(df) {
    df |>
      broom::tidy(parametric = TRUE) |>
      filter(str_detect(term, "decile")) |>
      # filter(str_detect(term, "quintile")) |>
      mutate(odds = exp(estimate)) |>
      mutate(lci = exp(estimate - 1.96*std.error)) |> 
      mutate(uci = exp(estimate + 1.96*std.error)) |> 
      select(term, odds, lci, uci)
    # pull(odds)
  })) |> 
  select(-c(data, model, value)) |>
  unnest(results) |>
  # # relocate(odds, .after = pdA) |> 
  # # relocate(std_error, .after = odds) |> 
  # # ADJUSTMENTS IF SMALLER SAMPLE SIZE FOR MODELS 13:28 (FACTOR OR 0.35/0.17):
  # # mutate(across(matches("^t"), ~ if_else(id %in% 13:28, . * (0.35 / 0.17), .))) %>%
  # saveRDS(str_c("from_ncdr_growth_attrb_v4_", min(.$id), "to", max(.$id), ".rds"))
  identity()

df_odds_results
future::futureSessionInfo()


# 10. PLOT ----------------------------------------------------------------

## a. decile -------------------------------------------------------------

df_odds_results |>
  # mutate(quint_dec = if_else(rwn %in% 1:2, "quint", "dec")) |> 
  mutate(term = (str_remove_all(term, "[:alpha:]|_"))) |> 
  mutate(term = if_else(term == 10, term, str_c(0, term))) |> 
  # mutate(term = as.factor(term)) |> 
  # mutate(term = fct_relevel(term, ""))
  
  # filter(InvestigationDescription != "Serologic test") |>
  # mutate(invst_group = as_factor(invst_group)) |> 
  mutate(term = as.integer(as.character(term))) |>
  ggplot() +
  geom_hline(yintercept = 1, lty = "dashed", colour = "grey20", alpha = 0.5) +
  # geom_pointrange(aes(reorder(InvestigationDescription, odds), odds, ymin = lci, ymax = uci), col = "grey40", stroke = NA)+
  geom_pointrange(aes(term, odds, ymin = lci, ymax = uci), col = "grey40", stroke = NA)+
  # geom_smooth(aes(term, odds), method = "lm")+
  theme_bw() +
  coord_flip() +
  scale_y_log10(
    # limits = c(0.85, 1.15),
    # breaks = c(0.9, 1, 1.1),
    # labels = c(
    #   # "Half\nas likely in\n2023/24",
    #   # "Equally\nas likely in\n2023/24",
    #   # "2x\nas likely in\n2023/24",
    #   "90%\nas likely as\nav. occupancy",
    #   "Equally\nas likely as\nav. occupancy",
    #   "110%x\nas likely as\nav. occupancy"
    # )
  )+
  theme(
    # axis.title.y = element_blank(),
    axis.title = element_text(size = 9),
    # plot.margin = margin(5, 20, 5, 5),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    strip.text = element_text(size = 9)
  ) +
  labs(
    x = "Occupany quintile (5 = highest occupancy)",
    y = "\nCase-mix-adjusted odds ratio of test (occupancy quintile vs 'average' occ) on log scale")+
  # facet_grid(invst_group ~ ., space = "free", scales = "free_y")
  # facet_grid(InvestigationDescription ~ quint_dec)
  facet_wrap(vars(InvestigationDescription))+
  NULL

## b. scaled value -------------------------------------------------------------
# THEY ARE JUST ORDERED FROM ED??? BECAUSE OCC IS HIGH? REMAIN IN ED BED?

df3$model[[4]] |> 
  broom::tidy(parametric = TRUE) |> 
  filter(str_detect(term, "scaled")) |>
  mutate(odds = exp(estimate)) |>
  mutate(lci = exp(estimate - 1.96*std.error)) |> 
  mutate(uci = exp(estimate + 1.96*std.error)) |> 
  select(term, odds, lci, uci)


# df_odds_results <- df3 |>
df_odds_results <- df3 |>
  bind_rows(df3a) |> 
  mutate(results = map(model, function(df) {
    df |>
      broom::tidy(parametric = TRUE) |>
      filter(str_detect(term, "scaled")) |>
      mutate(odds = exp(estimate)) |>
      mutate(lci = exp(estimate - 1.96*std.error)) |> 
      mutate(uci = exp(estimate + 1.96*std.error)) |> 
      select(term, odds, lci, uci)
    # pull(odds)
  })) |> 
  mutate(results_5pc = map(model, function(df) {
    df |>
      broom::tidy(parametric = TRUE) |>
      filter(str_detect(term, "scaled")) |>
      mutate(odds = exp(0.05*estimate)) |>
      # CHECK
      mutate(lci = exp(0.05*estimate - 0.05*1.96*std.error)) |> 
      mutate(uci = exp(0.05*estimate + 0.05*1.96*std.error)) |> 
      select(term, odds, lci, uci)
    # pull(odds)
  })) |> 
  select(-c(data, model, value)) |>
  unnest(results_5pc) |>
  # # relocate(odds, .after = pdA) |> 
  # # relocate(std_error, .after = odds) |> 
  # # ADJUSTMENTS IF SMALLER SAMPLE SIZE FOR MODELS 13:28 (FACTOR OR 0.35/0.17):
  # # mutate(across(matches("^t"), ~ if_else(id %in% 13:28, . * (0.35 / 0.17), .))) %>%
  # saveRDS(str_c("from_ncdr_growth_attrb_v4_", min(.$id), "to", max(.$id), ".rds"))
  identity()

df_odds_results |> 
  unnest(results, names_repair = "universal") |> 
  saveRDS("tmp_df_odds_results.rds")


df_odds_results |>
  # mutate(quint_dec = if_else(rwn %in% 1:2, "quint", "dec")) |> 
  mutate(term = (str_remove_all(term, "[:alpha:]|_"))) |> 
  mutate(term = if_else(term == 10, term, str_c(0, term))) |> 
  # mutate(term = as.factor(term)) |> 
  # mutate(term = fct_relevel(term, ""))
  
  # filter(InvestigationDescription != "Serologic test") |>
  # mutate(invst_group = as_factor(invst_group)) |> 
  mutate(term = as.integer(as.character(term))) |>
  ggplot() +
  geom_hline(yintercept = 0, lty = "dashed", colour = "grey20", alpha = 0.5) +
  geom_pointrange(aes(reorder(InvestigationDescription, odds-1), odds-1, ymin = lci-1, ymax = uci-1), col = "grey40", stroke = NA)+
  # geom_smooth(aes(term, odds), method = "lm")+
  theme_bw() +
  coord_flip() +
  scale_y_log10(
    # limits = c(0.85, 1.15),
    # breaks = c(0.9, 1, 1.1),
    # labels = c(
    #   # "Half\nas likely in\n2023/24",
    #   # "Equally\nas likely in\n2023/24",
    #   # "2x\nas likely in\n2023/24",
    #   "90%\nas likely as\nav. occupancy",
    #   "Equally\nas likely as\nav. occupancy",
    #   "110%x\nas likely as\nav. occupancy"
    # )
  )+
  theme(
    # axis.title.y = element_blank(),
    axis.title = element_text(size = 9),
    # plot.margin = margin(5, 20, 5, 5),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    strip.text = element_text(size = 9)
  ) +
  scale_y_continuous(labels = scales::percent_format())+
  labs(
    x = "Occupany quintile (5 = highest occupancy)",
    # y = "\nCase-mix-adjusted odds ratio of test (occupancy quintile vs 'average' occ) on log scale")+
    y = "\nIncrese in odds of test if a provider's bed occupancy level is 5% higher than average at a given time (case-mix-adjusted, shown on log scale)")+
  # facet_grid(invst_group ~ ., space = "free", scales = "free_y")
  # facet_grid(InvestigationDescription ~ quint_dec)
  # facet_wrap(vars(InvestigationDescription))+
  NULL


